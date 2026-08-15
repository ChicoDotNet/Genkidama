package io.genkidama.learn.java.helpdesk.http;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import io.genkidama.learn.java.helpdesk.domain.InvalidTicketTransitionException;
import io.genkidama.learn.java.helpdesk.domain.Ticket;
import io.genkidama.learn.java.helpdesk.domain.TicketNotFoundException;
import io.genkidama.learn.java.helpdesk.domain.TicketPriority;
import io.genkidama.learn.java.helpdesk.domain.TicketQuery;
import io.genkidama.learn.java.helpdesk.domain.TicketService;
import io.genkidama.learn.java.helpdesk.domain.TicketStatus;
import io.genkidama.learn.java.helpdesk.persistence.TicketPersistenceException;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.LongSupplier;

/** Minimal concurrent HTTP adapter for the HelpDesk ticket domain. */
public final class HelpDeskHttpServer implements AutoCloseable {
    private static final int HTTP_WORKERS = 4;
    private static final int MAX_JSON_BODY_BYTES = 64 * 1024;

    private final TicketService tickets;
    private final ObjectMapper json;
    private final HttpServer server;
    private final ExecutorService executor;
    private final RequestMetrics metrics = new RequestMetrics();
    private final boolean diagnosticsEnabled;
    private final LongSupplier nanoTime;

    /**
     * Creates a server with diagnostics disabled by default.
     * @param tickets domain service
     * @param json JSON mapper used only at the HTTP boundary
     * @param port TCP port; use {@code 0} in tests for an ephemeral port
     * @throws IOException when the socket cannot be created
     */
    public HelpDeskHttpServer(TicketService tickets, ObjectMapper json, int port) throws IOException {
        this(tickets, json, port, false);
    }

    /**
     * Creates a server with a bounded worker pool and optional aggregate diagnostics.
     * @param tickets domain service
     * @param json JSON mapper used only at the HTTP boundary
     * @param port TCP port; use {@code 0} in tests for an ephemeral port
     * @param diagnosticsEnabled exposes aggregate diagnostics when true
     * @throws IOException when the socket cannot be created
     */
    public HelpDeskHttpServer(TicketService tickets, ObjectMapper json, int port, boolean diagnosticsEnabled)
            throws IOException {
        this(tickets, json, port, diagnosticsEnabled, System::nanoTime);
    }

    HelpDeskHttpServer(
            TicketService tickets,
            ObjectMapper json,
            int port,
            boolean diagnosticsEnabled,
            LongSupplier nanoTime) throws IOException {
        this.tickets = tickets;
        this.json = json;
        this.diagnosticsEnabled = diagnosticsEnabled;
        this.nanoTime = nanoTime;
        this.server = HttpServer.create(new InetSocketAddress(port), 0);
        this.executor = Executors.newFixedThreadPool(HTTP_WORKERS);
        this.server.setExecutor(executor);
        this.server.createContext("/health", measured(this::handleHealth));
        this.server.createContext("/api/tickets", measured(this::handleTickets));
        if (diagnosticsEnabled) {
            this.server.createContext("/api/diagnostics", measured(this::handleDiagnostics));
        }
    }

    /** Starts accepting HTTP requests. */
    public void start() { server.start(); }

    /** Returns the actual bound port. */
    public int port() { return server.getAddress().getPort(); }

    @Override
    public void close() {
        server.stop(0);
        executor.shutdownNow();
    }

    private HttpHandler measured(HttpHandler handler) {
        return exchange -> {
            long started = nanoTime.getAsLong();
            try {
                handler.handle(exchange);
            } finally {
                metrics.recordDuration(Math.max(0L, nanoTime.getAsLong() - started));
            }
        };
    }

    private void handleHealth(HttpExchange exchange) throws IOException {
        if (!"GET".equals(exchange.getRequestMethod())) {
            send(exchange, 405, new ErrorResponse("method not allowed"));
            return;
        }
        send(exchange, 200, Map.of("status", "ok"));
    }

    private void handleDiagnostics(HttpExchange exchange) throws IOException {
        if (!diagnosticsEnabled) {
            send(exchange, 404, new ErrorResponse("route not found"));
            return;
        }
        if (!"GET".equals(exchange.getRequestMethod())) {
            send(exchange, 405, new ErrorResponse("method not allowed"));
            return;
        }
        send(exchange, 200, Map.of(
                "requests", metrics.snapshot(),
                "tickets", tickets.summary()));
    }

    private void handleTickets(HttpExchange exchange) throws IOException {
        try {
            String path = exchange.getRequestURI().getPath();
            if ("/api/tickets".equals(path)) {
                handleCollection(exchange);
                return;
            }
            if ("/api/tickets/summary".equals(path)) {
                if (!"GET".equals(exchange.getRequestMethod())) {
                    send(exchange, 405, new ErrorResponse("method not allowed"));
                    return;
                }
                send(exchange, 200, tickets.summary());
                return;
            }
            if (path.matches("/api/tickets/\\d+/advance")) {
                handleAdvance(exchange, path);
                return;
            }
            if (path.matches("/api/tickets/\\d+/priority")) {
                handlePriority(exchange, path);
                return;
            }
            send(exchange, 404, new ErrorResponse("route not found"));
        } catch (HttpInputException exception) {
            send(exchange, exception.statusCode(), new ErrorResponse(exception.getMessage()));
        } catch (TicketNotFoundException exception) {
            send(exchange, 404, new ErrorResponse(exception.getMessage()));
        } catch (InvalidTicketTransitionException exception) {
            send(exchange, 409, new ErrorResponse(exception.getMessage()));
        } catch (TicketPersistenceException exception) {
            send(exchange, 503, new ErrorResponse("ticket state is temporarily unavailable"));
        } catch (IllegalArgumentException | NullPointerException | JsonProcessingException exception) {
            send(exchange, 400, new ErrorResponse(exception.getMessage()));
        }
    }

    private void handleCollection(HttpExchange exchange) throws IOException {
        switch (exchange.getRequestMethod()) {
            case "GET" -> send(exchange, 200, tickets.list(parseQuery(exchange.getRequestURI().getRawQuery())));
            case "POST" -> {
                CreateTicketRequest request = readJson(exchange, CreateTicketRequest.class);
                Ticket created = tickets.create(request.title(), request.description(), request.priority());
                send(exchange, 201, created);
            }
            default -> send(exchange, 405, new ErrorResponse("method not allowed"));
        }
    }

    private void handleAdvance(HttpExchange exchange, String path) throws IOException {
        if (!"POST".equals(exchange.getRequestMethod())) {
            send(exchange, 405, new ErrorResponse("method not allowed"));
            return;
        }
        long id = idFrom(path, "/advance");
        send(exchange, 200, tickets.advance(id));
    }

    private void handlePriority(HttpExchange exchange, String path) throws IOException {
        if (!"PUT".equals(exchange.getRequestMethod())) {
            send(exchange, 405, new ErrorResponse("method not allowed"));
            return;
        }
        UpdatePriorityRequest request = readJson(exchange, UpdatePriorityRequest.class);
        long id = idFrom(path, "/priority");
        send(exchange, 200, tickets.changePriority(id, request.priority()));
    }

    private <T> T readJson(HttpExchange exchange, Class<T> type) throws IOException {
        String contentType = exchange.getRequestHeaders().getFirst("Content-Type");
        if (contentType == null || !"application/json".equalsIgnoreCase(contentType.split(";", 2)[0].strip())) {
            throw new HttpInputException(415, "Content-Type application/json is required");
        }
        String contentLength = exchange.getRequestHeaders().getFirst("Content-Length");
        if (contentLength != null) {
            try {
                if (Long.parseLong(contentLength) > MAX_JSON_BODY_BYTES) {
                    throw new HttpInputException(413, "JSON body exceeds 64 KiB limit");
                }
            } catch (NumberFormatException exception) {
                throw new HttpInputException(400, "invalid Content-Length");
            }
        }
        byte[] payload = exchange.getRequestBody().readNBytes(MAX_JSON_BODY_BYTES + 1);
        if (payload.length > MAX_JSON_BODY_BYTES) {
            throw new HttpInputException(413, "JSON body exceeds 64 KiB limit");
        }
        return json.readValue(payload, type);
    }

    private static long idFrom(String path, String suffix) {
        String rawId = path.substring("/api/tickets/".length(), path.length() - suffix.length());
        return Long.parseLong(rawId);
    }

    private static TicketQuery parseQuery(String rawQuery) {
        if (rawQuery == null || rawQuery.isBlank()) return TicketQuery.all();
        Map<String, String> values = new LinkedHashMap<>();
        for (String pair : rawQuery.split("&")) {
            String[] parts = pair.split("=", 2);
            if (parts.length == 2) {
                values.put(decode(parts[0]), decode(parts[1]));
            }
        }
        TicketStatus status = parseEnum(values.get("status"), TicketStatus.class);
        TicketPriority priority = parseEnum(values.get("priority"), TicketPriority.class);
        return new TicketQuery(status, priority);
    }

    private static String decode(String value) {
        return URLDecoder.decode(value, StandardCharsets.UTF_8);
    }

    private static <E extends Enum<E>> E parseEnum(String value, Class<E> type) {
        if (value == null || value.isBlank()) return null;
        try {
            return Enum.valueOf(type, value.strip().toUpperCase(Locale.ROOT));
        } catch (IllegalArgumentException exception) {
            throw new IllegalArgumentException("invalid " + type.getSimpleName() + ": " + value, exception);
        }
    }

    private void send(HttpExchange exchange, int statusCode, Object body) throws IOException {
        byte[] payload = json.writeValueAsString(body).getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().set("Content-Type", "application/json; charset=utf-8");
        exchange.getResponseHeaders().set("X-Content-Type-Options", "nosniff");
        exchange.getResponseHeaders().set("Referrer-Policy", "no-referrer");
        exchange.getResponseHeaders().set("Content-Security-Policy", "default-src 'none'; frame-ancestors 'none'; base-uri 'none'");
        exchange.sendResponseHeaders(statusCode, payload.length);
        try (var output = exchange.getResponseBody()) {
            output.write(payload);
        } finally {
            metrics.record(statusCode);
        }
    }

    private static final class HttpInputException extends IllegalArgumentException {
        private final int statusCode;

        private HttpInputException(int statusCode, String message) {
            super(message);
            this.statusCode = statusCode;
        }

        private int statusCode() { return statusCode; }
    }
}
