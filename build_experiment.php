<?php
$post_data = json_decode(file_get_contents('php://input'), true); 
// the directory "data" must be writable by the server
$name = "/srv/experiment-data/doxa1/".$post_data['filename'].".csv"; 
$data = $post_data['filedata'];
// write the file to disk
system("chmod o+w /srv/www/html/experiments/exps/");
exec('touch /srv/www/html/experiments/exps/aaa');
system('chmod o-w /srv/www/html/experiments/exps/');
?>
