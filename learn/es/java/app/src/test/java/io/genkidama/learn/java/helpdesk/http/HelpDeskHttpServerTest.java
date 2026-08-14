package io.genkidama.learn.java.helpdesk.http;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.genkidama.learn.java.helpdesk.domain.TicketService;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.concurrent.atomic.AtomicLong;

import static org.junit.jupiter.api.Assertions.assertEquals;

class HelpDeskHttpServerTest {
    private final ObjectMapper json = new ObjectMapper();
    private final HttpClient client = HttpClient.newHttpClient();

    @Test
    void healthEndpointProvesTheApplicationCanServeHttpWithDefensiveHeaders() throws Exception {
        try (var server = startedServer(false)) {
            HttpResponse<String> response = get(server, "/health");
            assertEquals(200, response.statusCode());
            assertEquals("ok", json.readTree(response.body()).get("status").asText());
            assertEquals("nosniff", response.headers().firstValue("X-Content-Type-Options").orElseThrow());
            assertEquals("no-referrer", response.headers().firstValue("Referrer-Policy").orElseThrow());
            assertEquals("default-src 'none'; frame-ancestors 'none'; base-uri 'none'",
                    response.headers().firstValue("Content-Security-Policy").orElseThrow());
        }
    }

    @Test
    void createsFiltersUpdatesAdvancesAndSummarizesTicketsThroughJsonApi() throws Exception {
        try (var server = startedServer(false)) {
            HttpResponse<String> created = sendJson(server, "POST", "/api/tickets", """
                    {"title":"VPN caída","description":"Sin acceso remoto","priority":"HIGH"}
                    """);
            assertEquals(201, created.statusCode());

            sendJson(server, "POST", "/api/tickets", """
                    {"title":"Monitor","priority":"LOW"}
                    """);

            HttpResponse<String> filtered = get(server, "/api/tickets?priority=high&status=open");
            assertEquals(200, filtered.statusCode());
            assertEquals(1, json.readTree(filtered.body()).size());

            HttpResponse<String> priority = sendJson(server, "PUT", "/api/tickets/1/priority", """
                    {"priority":"NORMAL"}
                    """);
            assertEquals(200, priority.statusCode());
            assertEquals("NORMAL", json.readTree(priority.body()).get("priority").asText());

            HttpResponse<String> advanced = sendJson(server, "POST", "/api/tickets/1/advance", "{}");
            assertEquals(200, advanced.statusCode());
            assertEquals("IN_PROGRESS", json.readTree(advanced.body()).get("status").asText());

            JsonNode summary = json.readTree(get(server, "/api/tickets/summary").body());
            assertEquals(2, summary.get("total").asLong());
            assertEquals(1, summary.get("open").asLong());
            assertEquals(1, summary.get("inProgress").asLong());
        }
    }

    @Test
    void diagnosticsAreOptInAggregateAndUseMonotonicTiming() throws Exception {
        AtomicLong clock = new AtomicLong();
        try (var server = startedServer(true, () -> clock.getAndAdd(1_000_000L))) {
            assertEquals(200, get(server, "/health").statusCode());

            JsonNode diagnostics = json.readTree(get(server, "/api/diagnostics").body());
            assertEquals(1, diagnostics.get("requests").get("requests").asLong());
            assertEquals(0, diagnostics.get("requests").get("failures").asLong());
            assertEquals(1_000_000L, diagnostics.get("requests").get("totalDurationNanos").asLong());
            assertEquals(0, diagnostics.get("tickets").get("total").asLong());
        }
    }

    @Test
    void rejectsWrongMediaTypeAndOversizedJsonBeforeMutatingState() throws Exception {
        try (var server = startedServer(false)) {
            HttpResponse<String> wrongType = send(server, "POST", "/api/tickets",
                    "text/plain", "{\"title\":\"No debe entrar\",\"priority\":\"NORMAL\"}");
            assertEquals(415, wrongType.statusCode());

            String oversized = "{\"title\":\"" + "x".repeat(70_000) + "\",\"priority\":\"NORMAL\"}";
            HttpResponse<String> tooLarge = send(server, "POST", "/api/tickets", "application/json", oversized);
            assertEquals(413, tooLarge.statusCode());

            assertEquals(0, json.readTree(get(server, "/api/tickets").body()).size());
        }
    }

    @Test
    void translatesDomainAndInputFailuresToUsefulHttpStatuses() throws Exception {
        try (var server = startedServer(false)) {
            HttpResponse<String> invalid = sendJson(server, "POST", "/api/tickets", """
                    {"title":"   ","priority":"NORMAL"}
                    """);
            assertEquals(400, invalid.statusCode());

            assertEquals(400, get(server, "/api/tickets?status=unknown").statusCode());
            assertEquals(404, sendJson(server, "POST", "/api/tickets/999/advance", "{}").statusCode());
        }
    }

    private HelpDeskHttpServer startedServer(boolean diagnostics) throws Exception {
        var server = new HelpDeskHttpServer(new TicketService(), json, 0, diagnostics);
        server.start();
        return server;
    }

    private HelpDeskHttpServer startedServer(boolean diagnostics, java.util.function.LongSupplier clock) throws Exception {
        var server = new HelpDeskHttpServer(new TicketService(), json, 0, diagnostics, clock);
        server.start();
        return server;
    }

    private HttpResponse<String> get(HelpDeskHttpServer server, String path) throws Exception {
        return client.send(HttpRequest.newBuilder(uri(server, path)).GET().build(), HttpResponse.BodyHandlers.ofString());
    }

    private HttpResponse<String> sendJson(HelpDeskHttpServer server, String method, String path, String body) throws Exception {
        return send(server, method, path, "application/json", body);
    }

    private HttpResponse<String> send(
            HelpDeskHttpServer server, String method, String path, String contentType, String body) throws Exception {
        return client.send(
                HttpRequest.newBuilder(uri(server, path))
                        .header("Content-Type", contentType)
                        .method(method, HttpRequest.BodyPublishers.ofString(body))
                        .build(),
                HttpResponse.BodyHandlers.ofString());
    }

    private URI uri(HelpDeskHttpServer server, String path) {
        return URI.create("http://127.0.0.1:" + server.port() + path);
    }
}
