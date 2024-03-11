<?php
    function customRequirePath($relative_path) {
        $current_dir = __DIR__;
        return $current_dir."/".$relative_path;
    }

    require_once "constants.php";
    require_once "vendor/autoload.php";
    require_once "api/client.php";
?>