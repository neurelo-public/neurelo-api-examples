<?php
require_once '../require.php';

// Read all get variables
$search = $_GET['search'] ?? null;
$page = $_GET['page'] ?? 1;

require_once '../layouts/header.php';
?>

<div class="max-container pt-3 flex flex-nowrap gap-2 items-center justify-between mx-auto">

    <form method="GET" action="<?php echo $_SERVER['PHP_SELF']; ?>" class="justify-center items-center flex gap-2">
        <input autofocus type="text" value="<?php echo $search; ?>" name="search" placeholder="Search for Films">
        <button type="submit">Search</button>
    </form>

<?php
use Api\Client\ApiClient;

$client = new ApiClient();

$query = [
    'take' => RESULTS_PER_PAGE,
    'skip' => ($page - 1) * RESULTS_PER_PAGE,
];

if (!is_null($search)) {
    $query['filter'] = [
        "OR" => [
            [
                "title" => [
                    "contains" => $search
                ]
            ],
            [
                "description" => [
                    "contains" => $search
                ]
            ]
        ]
    ];
    $query['filter'] = json_encode($query['filter']);
}

$response = $client->request('GET', '/rest/film', $query);

$template = "
<div class='flex flex-nowrap gap-4 items-center justify-center'>
    <p><b>Status Code:</b> <span class='tag'>:status_code</span></p>
    <p><b>Size:</b> <span class='tag'>:size</span></p>
</div>
";

$template_var = [
    ':status_code' => $response->status_code,
    ':content_type' => $response->content_type,
    ':size' => $response->size
];

echo strtr($template, $template_var);
echo "</div>";

if (is_null($response->content)) {
    echo "N/A";
} else {
    $json_decoded = json_decode($response->content, true);

    if (!is_array($json_decoded["data"])) {
        echo "N/A";
    } else {
        echo "
        <div class='center-content-row flex flex-col gap-2 text-white'>
            <div class='w-full max-container code-block-container overflow-y-auto overflow-x-hidden'>
        ";

        $film_card_template = "
        <div class='card-container'>
            <a href=':url' class='w-full card'>
                <div class='w-full flex flex-col gap-3 items-start justify-center'>
                    <div class='w-full flex flex-col gap-1'>
                        <h4 class='text-white text-lg font-medium'>:title</h4>
                    </div>
                    <div class='w-full flex flex-col gap-1'>
                        <p class='text-slate-200 w-full text-md font-normal text-overflow-ellipses'>:description</p>
                    </div>
                    <div class='w-full flex justify-between items-center gap-1'>
                        <div class='w-full flex justify-start items-center gap-2 mt-2'>
                            <h3 class='text-slate-400 text-sm font-semibold font-sans text-uppercase'>Release Year:</h3>
                            <p class='text-white text-md font-normal'>:release_date</p>
                        </div>
                        <h3 class='text-slate-100 text-lg font-normal font-sans'>:rental_rate</h3>
                    </div>
                </div>
            </a>
        </div>";

        foreach ($json_decoded["data"] as $film) {
            /**
             * {
             *      "description": "A Epic Drama of a Feminist And a Mad Scientist who must Battle a",
             *      "film_id": 1,
             *      "language_id": 1,
             *      "last_update": "2024-02-29T01:57:37.808Z",
             *      "length": 86,
             *      "release_year": 2006,
             *      "rental_duration": 6,
             *      "rental_rate": "80.99",
             *      "replacement_cost": "20.99",
             *      "special_features": [
             *          "Deleted Scenes",
             *          "Behind the Scenes"
             *      ],
             *      "title": "Academy Dinosaur edit 1"
             *  }
             */
            echo strtr($film_card_template, [
                ':url' => "/form/edit-film.php?id=".$film["film_id"],
                ':title' => $film["title"],
                ':description' => $film["description"],
                ':release_date' => $film["release_year"],
                ':rental_rate' => "$".$film["rental_rate"]."<span style='color: var(--sky-500);' class='text-sm font-normal whitespace-nowrap font-sans'>/".$film['rental_duration']." day</span>"
            ]);
        }

        echo "</div>
        </div>";
    }

}

require_once '../layouts/footer.php';
?>