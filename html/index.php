<?php
header("Content-Type: text/html; charset=utf-8");
ini_set('display_errors', 1);
session_start();
require_once 'api/php/hawk_api.php';
use \hawk_api\hawk_api;

$api = new hawk_api('Nflz2H7VcQ6Rve8lo3NDAKavZL+lSmUd5CuhhN585iHN4XsuTsE8O5N2U823egECQNWkVpFq45TLfCl1rALHhw==', 'http://127.0.0.1:2222');
echo '<pre>';
print_r($api->register_user('test'));
echo "\n";
print_r($api->register_user('test2'));
echo "\n";
print_r($api->add_user_to_group('test2', array('89e5d263c387e3ace58486c1eaa3adc7')));
echo "\n";
print_r($api->add_user_to_group('test', array('testgroup', 'group1')));
echo "\n";
print_r($api->add_user_to_group('test2', array('group1')));
//echo "\n";
//print_r($api->remove_user_from_group('test', array('testgroup')));
echo "\n";
print_r($api->seng_group_message('test', 'tstt msg', array('testgroup', 'group1')));
echo "\n";
print_r(json_decode($api->get_user_by_group(array('89e5d263c387e3ace58486c1eaa3adc7'))));
/*echo "\n";
print_r(json_decode($api->get_user_by_group(array('testgroup'))));
echo "\n";
print_r(json_decode($api->get_user_by_group(array('testgroup', 'group1'))));*/


echo '</pre>';
?>
<!DOCTYPE html>
<html>
	<head>
		<title></title>
		<script type="text/javascript" src="api/js/jq.js"></script>
		<script type="text/javascript" src="api/js/hawk_api.js"></script>
		<script type="text/javascript" src="api/js/init.js"></script>
	</head>

	<body>

		<div id="messages" style="width: 500px; height: 250px; border: 2px solid green;overflow: auto;"></div>
		Кому: <input id="to_u" type="text"><br>
		Текст: <textarea id="to_m"></textarea><br>
		<input type="button" id="send" value="Отправить">
	</body>
</html>
