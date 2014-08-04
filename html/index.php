<?php
header("Content-Type: text/html; charset=utf-8");
ini_set('display_errors', 1);

require_once 'api/php/hawk_api.php';
use \hawk_api\hawk_api;

$api = new hawk_api('Nflz2H7VcQ6Rve8lo3NDAKavZL+lSmUd5CuhhN585iHN4XsuTsE8O5N2U823egECQNWkVpFq45TLfCl1rALHhw==');
echo '<pre>';
print_r($api->register_user('test'));
echo "\n";
print_r($api->register_user('test2'));
echo "\n";
print_r($api->add_user_to_group('test2', array('test_group', 'group1')));
echo "\n";
print_r($api->add_user_to_group('test', array('test_group', 'group1')));
echo "\n";
print_r($api->remove_user_from_group('test', array('test_group')));
echo "\n";
print_r($api->seng_group_message('test', 'tstt msg', array('test_group', 'group1')));
/*echo "\n";
print_r(json_decode($api->get_user_by_group(array('group1'))));
echo "\n";
print_r(json_decode($api->get_user_by_group(array('test_group'))));
echo "\n";
print_r(json_decode($api->get_user_by_group(array('test_group', 'group1'))));*/


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

		<div id="messages" style="width: 500px; height: 250px; border: 2px solid green;"></div>
		Кому: <input id="to_u" type="text"><br>
		Текст: <textarea id="to_m"></textarea><br>
		<input type="button" id="send" value="Отправить">
	</body>
</html>
