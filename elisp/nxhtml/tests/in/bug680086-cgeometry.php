<?php

if ( !defined('_VALID_MOS') )
{
	define( '_VALID_MOS', 1 );
	include_once("../../configuration.inc");
}

include_once($config->path."include/omf/database.php");
include_once($config->path."modules/map/cmap.php");
include_once($config->path."modules/layer/clayer.php");