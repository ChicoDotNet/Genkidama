package io.genkidama.learn.java.helpdesk.http;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.genkidama.learn.java.helpdesk.domain.TicketService;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;

class HelpDeskHttpServerTest {
    private final ObjectMapper json = new ObjectMapper();
    private final HttpClient client = HttpClient.newHttpClient();

    @Test
    void healthEndpointProvesTheApplicationCanServeHttp() throws Exception {
        try (var server = startedServer()) {
            HttpResponse<String> response = client.send(
                    HttpRequest.newBuilder(uri(server, "/health")).GET().build(),
                    HttpResponse.BodyHandlers.ofString());
            assertEquals(200, response.statusCode());
            assertEquals("ok", json.readTree(response.body()).get("status").asText());
        }
    }

    @Test
    void createsFiltersUpdatesAndAdvancesATicketThroughJsonApi() throws Exception {
        try (var server = startedServer()) {
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
        }
    }

    @Test
    void translatesDomainAndInputFailuresToUsefulHttpStatuses() throws Exception {
        try (var server = startedServer()) {
            HttpResponse<String> invalid = sendJson(server, "POST", "/api/tickets", """
                    {"title":"   ","priority":"NORMAL"}
                    """);
            assertEquals(400, invalid.statusCode());

            assertEquals(400, get(server, "/api/tickets?status=unknown").statusCode());
            assertEquals(404, sendJson(server, "POST", "/api/tickets/999/advance", "{}").statusCode());
        }
    }

    private HelpDeskHttpServer startedServer() throws Exception {
        var server = new HelpDeskHttpServer(new TicketService(), json, 0);
        server.start();
        return server;
    }

    private HttpResponse<String> get(HelpDeskHttpServer server, String path) throws Exception {
        return client.send(HttpRequest.newBuilder(uri(server, path)).GET().build(), HttpResponse.BodyHandlers.ofString());
    }

    private HttpResponse<String> sendJson(HelpDeskHttpServer server, String method, String path, String body) throws Exception {
        return client.send(
                HttpRequest.newBuilder(uri(server, path))
                        .header("Content-Type", "application/json")
                        .method(method, HttpRequest.BodyPublishers.ofString(body))
                        .build(),
                HttpResponse.BodyHandlers.ofString());
    }

    private URI uri(HelpDeskHttpServer server, String path) {
        return URI.create("http://127.0.0.1:" + server.port() + path);
    }
}
