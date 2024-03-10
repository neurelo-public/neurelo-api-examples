<?php
namespace Api\Client;

use ErrorException;
use GuzzleHttp\Client;

define('API_KEY_HEADER_KEY', 'X-API-KEY');
define('API_KEY_VALUE', getenv('NEURELO_API_KEY'));
define('API_BASE_PATH', getenv('NEURELO_API_BASE_PATH'));

class Response {
    public int $status_code;
    public ?string $content_type;
    public ?string $size;
    public ?string $content;

    public function __construct() {
        $this->status_code = 0;
        $this->content_type = null;
        $this->size = null;
        $this->content = null;
    }
}

class ApiClient {
    private ?Client $client;
    private ?array $headers;
    private ?string $base_path;

    public function __construct() {
        $this->headers = [API_KEY_HEADER_KEY => API_KEY_VALUE];
        $this->base_path = API_BASE_PATH;
        $this->client = new Client([
            'headers' => $this->headers
        ]);

        return $this;
    }

    public function request($method, $url, $query, $body = null) {
        if (empty($method)) {
            throw new ErrorException("Method is required", 500);
        }
        if (empty($url)) {
            $url = "";
        }
        if (empty($query)) {
            $query = [];
        }
        if (empty($this->base_path)) {
            throw new ErrorException("Base path is required", 500);
        }

        $full_url = $this->base_path.$url;

        $response = $this->client->request($method, $full_url, [
            'query' => $query,
            'body' => $body
        ]);

        $responseObj = new Response();
        $responseObj->status_code = $response->getStatusCode();
        $responseObj->content_type = mb_split("; ", $response->getHeaderLine('content-type'))[0];
        $responseObj->content = $response->getBody()->getContents();

        $response_bytes = mb_strlen($responseObj->content, '8bit');
        $response_kb = "N/A";
        if (!empty($responseObj->content)) {
            $response_kb = number_format($response_bytes / 1024, 2)." KB";
        }
        $responseObj->size = $response_kb;

        return $responseObj;
    }
}