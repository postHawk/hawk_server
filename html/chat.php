<?php
header("Content-Type: text/html; charset=utf-8");
ini_set('display_errors', 1);

require_once 'api/hawk_chat.php';
hawk_chat::session_start();

$chat = new hawk_chat('Nflz2H7VcQ6Rve8lo3NDAKavZL+lSmUd5CuhhN585iHN4XsuTsE8O5N2U823egECQNWkVpFq45TLfCl1rALHhw==');

$action = (isset($_POST['action']) ? $_POST['action'] : false);
if($action)
{
	$chat->ajax($action);
}
else
{
	$user = $chat->get_user();
	echo '<script type="text/javascript">
		var CONTROL_OBJ = {
			user_id: \'' . $user['id'] . '\',
			group_id: \'' . $chat->get_group_id() . '\',
			user_login: \'' . $user['login'] . '\'
		};
		</script>';
}
?>

<!DOCTYPE html>
<html>
	<head>
		<title></title>
		<script type="text/javascript" src="api/js/jq.js"></script>
		<script type="text/javascript" src="api/js/ui/jquery-ui.min.js"></script>
		<script type="text/javascript" src="api/js/hawk_api.js"></script>
		<script type="text/javascript" src="api/js/chat.js"></script>
		<link rel="stylesheet" type="text/css" href="api/js/ui/jquery-ui.min.css"/>
		<link rel="stylesheet" type="text/css" href="api/js/ui/jquery-ui.structure.min.css"/>
		<link rel="stylesheet" type="text/css" href="api/js/ui/jquery-ui.theme.min.css"/>

		<style>
			.online{
				color: green;
			}

			.offline{
				color: red;
			}

			.separator{
				font-style: italic;
			}
		</style>

	</head>

	<body style="margin: 0px; padding: 0px; position: absolute;">
		<div id="wrapper" style="position: fixed; width: 100%; height: 100%;z-index: 2;">
			<div id="chat" style="position: absolute; bottom: 0px; right: 0px; width: 400px; height: 300px; border: 5px solid #569bd8; border-radius: 5px; background-color: white;">
				<div id="header" style="text-align: center; position: relative; height: 20px; top: 0px;border-bottom: 3px solid #569bd8; background-color: #aecde7">
					<span style="padding: 2px;">Hawk chat</span>
				</div>
				<div style="display: inline-block; position: absolute; width: inherit; height: inherit;">
					<div id="users" style="position: absolute; width: 150px; height: 280px; overflow: auto; left: 0px;border-right: 3px solid #569bd8;">
						<div class="separator" style="font-size: 14px;"><b>ONLINE</b></div>
						<div id="online_u"></div>
						<div class="separator" style="font-size: 14px;"><b>OFFLINE</b></div>
						<div id="offline_u"></div>
					</div>
					<div style="position: absolute;width: 250px; right: 0px; height: 280px;">
						<div id="messages" style="overflow: auto; height: 245px; ;padding: 5px"></div>
						<div id="out_text" contenteditable="true" style="position: absolute; width: inherit; height: 35px; overflow: auto; bottom: 0px;border-top: 3px solid #569bd8;padding-left: 5px;	padding-top: 5px;"></div>
						<span style="padding: 2px; position: absolute; left: 5px; bottom: 45px;" title="отправка сообщения по Ctrl+Enter"><b><i>?</i></b></span>
					</div>
				</div>
			</div>
		</div>
		<div id="action_log" style="z-index: 1; width: 500px; height: 500px; position: absolute; overflow: auto; border: 3px solid black; border-radius: 5px;">
			Лог<br>
		</div>
	</body>
</html>