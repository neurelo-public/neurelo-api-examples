<?php
    require_once '../require.php';
    use Api\Client\ApiClient;

    $client = new ApiClient();

    $is_get = $_SERVER['REQUEST_METHOD'] === 'GET';
    $is_post = $_SERVER['REQUEST_METHOD'] === 'POST';

    $id = $_GET['id'] ?? $_POST['id'];
    $error = null;

    if (is_null($id)) {
        // Fix header already sent error
        header('LOCATION: /form/search-films.php');
        exit;
    }

    if ($is_get) {
        $response = $client->request('GET', '/rest/film/'.$id, []);

        $film = json_decode($response->content)->data;

        if (is_null($film)) {
            header('LOCATION: /form/search-films.php');
            exit;
        }
    } else if ($is_post) {
        $title = null;
        $description = null;
        $rental_rate = null;
        $rental_duration = null;

        try {
            $title = $_POST['title'];
            $description = $_POST['description'];
            $rental_rate = $_POST['rental_rate'];
            $rental_duration = $_POST['rental_duration'];

            $response = $client->request('PATCH', '/rest/film/'.$id, null,
                json_encode([
                    'title' => $title,
                    'description' => $description,
                    'rental_rate' => $rental_rate,
                    'rental_duration' => intval($rental_duration),
                ], JSON_UNESCAPED_SLASHES));

            if ($response->status_code === 200) {
                header('LOCATION: /form/search-films.php');
                $error = null;
                exit;
            }
            $error = "Failed to update film";
        } catch (Exception $e) {
            $title = null;
            $description = null;
            $rental_rate = null;
            $rental_duration = null;
            $error = null;
            $error = $e->getMessage();
        }
    }

    require_once '../layouts/header.php';
?>

    <div class="max-container mx-auto p-4 flex flex-row items-center justify-between gap-2">
        <h1 class="text-xl font-semibold text-white py-2 text-center">Edit Film</h1>
        <p class="text-md font-normal text-slate-300 py-2 text-center">ID: <?php echo $id ?></p>
    </div>

    <?php
    if (!is_null($error)) {
        echo "
        <div class='max-w-md mx-auto p-4 flex items-center justify-center gap-2'>
            <p class='text-red-500'>$error</p>
        </div>
        ";
    }
    ?>

    <form
        method="POST"
        action="<?php echo $_SERVER['PHP_SELF']."?id=".$id; ?>"
        class="max-w-md pt-3 mx-auto flex flex-col gap-3 items-stretch"
    >
        
        <fieldset>
            <label for="title">Title</label>
            <input autofocus require type="text" id="title" name="title" value="<?php echo !is_null($film) ? $film->title : null; ?>" />
        </fieldset>

        <fieldset>
            <label for="description">Description</label>
            <textarea require id="description" name="description" style="height: 8rem;"><?php echo !is_null($film) ? $film->description : null; ?></textarea>
        </fieldset>

        <fieldset>
            <label for="rental_rate">Rental Rate</label>
            <input require type="number" min="0.00" max="100.00" step="0.01" id="rental_rate" name="rental_rate" value="<?php echo !is_null($film) ? $film->rental_rate : null; ?>" />
        </fieldset>

        <fieldset>
            <label for="rental_duration">Rental Duration</label>
            <input require type="number" min="0" max="31" step="1" id="rental_duration" name="rental_duration" value="<?php echo !is_null($film) ? $film->rental_duration : null; ?>" />
        </fieldset>

        <div class="flex flex-nowrap items-center justify-between">
            <button type="reset">
                Reset
            </button>
            
            <button type="submit">
                Save
            </button>
        </div>
    </form>
<?php
require_once '../layouts/footer.php';
?>