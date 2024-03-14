package com.example.demo.client;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpClient.Redirect;
import java.net.http.HttpClient.Version;
import java.net.http.HttpResponse.BodyHandlers;
import java.time.Duration;
import java.util.HashMap;

public class ApiClient {
    public ApiConfig apiConfig;

    private HttpClient client = null;
    private HttpRequest request;

    private String baseUrl;
    private String apiKey;

    private int statusCode;
    private String body;

    public ApiClient(ApiConfig apiConfig) {
        client = HttpClient.newBuilder()
                .version(Version.HTTP_2)
                .followRedirects(Redirect.NORMAL)
                .build();
        baseUrl = apiConfig.getBaseUrl();
        apiKey = apiConfig.getKey();
    }

    public HashMap<Number, String> call(String url, String method, String reqBody) {
        String fullUrl = baseUrl + url;

        if (method == "GET" && reqBody != null) {
            fullUrl = fullUrl + "?" + reqBody;
        }

        System.out.println("Base baseUrl+url: " + fullUrl);

        HttpRequest.Builder requestL = HttpRequest.newBuilder()
                .uri(URI.create(fullUrl).normalize())
                .timeout(Duration.ofMinutes(1))
                .header("Content-Type", "application/json")
                .header("X-API-KEY", apiKey);

        if (reqBody != null && method != "GET") {
            request = requestL.method(method, HttpRequest.BodyPublishers.ofString(reqBody)).build();
        } else {
            request = requestL.method(method, HttpRequest.BodyPublishers.noBody()).build();
        }

        try {
            HttpResponse<String> response = client.send(request, BodyHandlers.ofString());
            statusCode = response.statusCode();
            body = response.body();

            System.out.println("Status code: " + statusCode);
            System.out.println("Body: " + body);

            HashMap<Number, String> returnMap = new HashMap<Number, String>();
            returnMap.put(statusCode, body);
            return returnMap;
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();

            HashMap<Number, String> returnMap = new HashMap<Number, String>();
            returnMap.put(500, null);
            return returnMap;
        }
    }
}
