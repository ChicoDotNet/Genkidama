package io.genkidama.learn.java.helpdesk.http;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;
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

/** Minimal concurrent HTTP adapter for the HelpDesk ticket domain. */
public final class HelpDeskHttpServer implements AutoCloseable {
    private static final int HTTP_WORKERS = 4;

    private final TicketService tickets;
    private final ObjectMapper json;
    private final HttpServer server;
    private final ExecutorService executor;
    private final RequestMetrics metrics = new RequestMetrics();
    private final boolean diagnosticsEnabled;

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
        this.tickets = tickets;
        this.json = json;
        this.diagnosticsEnabled = diagnosticsEnabled;
        this.server = HttpServer.create(new InetSocketAddress(port), 0);
        this.executor = Executors.newFixedThreadPool(HTTP_WORKERS);
        this.server.setExecutor(executor);
        this.server.createContext("/health", this::handleHealth);
        this.server.createContext("/api/tickets", this::handleTickets);
        if (diagnosticsEnabled) {
            this.server.createContext("/api/diagnostics", this::handleDiagnostics);
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
                CreateTicketRequest request = json.readValue(exchange.getRequestBody(), CreateTicketRequest.class);
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
        UpdatePriorityRequest request = json.readValue(exchange.getRequestBody(), UpdatePriorityRequest.class);
        long id = idFrom(path, "/priority");
        send(exchange, 200, tickets.changePriority(id, request.priority()));
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
        exchange.sendResponseHeaders(statusCode, payload.length);
        try (var output = exchange.getResponseBody()) {
            output.write(payload);
        } finally {
            metrics.record(statusCode);
        }
    }
}
