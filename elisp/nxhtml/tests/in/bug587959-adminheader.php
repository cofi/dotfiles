<?php

// Calculate what the header will be.
$sess = Context::session();

if ( ! $agency = Finder::find_by_association( 'Agency', get_user(), 'agency_student' ) )
{
  $background = "background-image: url('" . AccessHelper::get_path() . "/public/images/event_manager.png');_background-image: none;_filter:progid:DXImageTransform.Microsoft.AlphaImageLoader(src='" . AccessHelper::get_path() . "/public/images/event_manager.png',sizingMethod='image');";
}
else
{
  $agency = $agency[0];
  $background = "background-repeat: no-repeat; background-image: url('" . Agency::get_theme_image ( $agency->attrib($agency->id_field), 'header_logo.png' ) . "');_background-image: none;_filter:progid:DXImageTransform.Microsoft.AlphaImageLoader(src='" . Agency::get_theme_image ( $agency->attrib($agency->id_field), 'header_logo.png' ) . "',sizingMethod='image');";
}

echo "<" . "?xml version=\"1.0\" encoding=\"utf-8\"?" . ">"; ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
        "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">

<head>
  <title>Event Registration <?php echo ( ( $t = get("page_title") ) ? ": " . $t : "" ); ?></title>
  <meta http-equiv="content-type" content="text/html;charset=utf-8" />
  <meta http-equiv="Content-Style-Type" content="text/css" />
  <link rel="stylesheet" type="text/css" href="<?php echo AccessHelper::get_path(); ?>/public/style/screen.css" media="screen" />
  <link rel="stylesheet" type="text/css" href="<?php echo AccessHelper::get_path(); ?>/public/style/print.css" media="print" />
  <script type="text/javascript" src="<?php echo AccessHelper::get_path(); ?>/js/korrector.js">No Script</script>
  <script type="text/javascript" src="<?php echo AccessHelper::get_path(); ?>/js/zebra.js">No Script</script>
  <script type="text/javascript" src="<?php echo AccessHelper::get_path(); ?>/js/prototype.js"> </script>
  <script type="text/javascript" src="<?php echo AccessHelper::get_path(); ?>/js/window.js"> </script>
  <link href="<?php echo AccessHelper::get_path(); ?>/public/kalendar/themes/default.css" rel="stylesheet" type="text/css" />
  <link href="<?php echo AccessHelper::get_path(); ?>/public/kalendar/themes/alphacube.css" rel="stylesheet" type="text/css" />

<script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false"></script>
  <script type="text/javascript">
  <!--
  window.onload = stripe;
  -->
  </script>

<?php echo render_additional_header(); ?>

</head>

<body>

  <div class="stage">

    <div class="contentHeader">
      <h1 class="logo" style="width:500px;<?php echo $background; ?>">
        <?php echo ( $t = get("page_title") ) ? $t : "Event Registration"; ?>
      </h1>
      <div class="loginBox">
        
        <?php include_partial( 'global/loginbox' ); ?>
        
      </div>
      
    </div>

    <div class="content">
      <?php
        $t = '';
        $session = Context::session();
        if ( $choose_event_template = $session->get('admin_choose_event_template') )
        {
          $et = Finder::find_by_id ( 'EventTemplate', $choose_event_template );
          $t  = $et->attrib('event_name');
        }
        if ( $t == '' )
        {
          $t = get('page_title');
        }
        echo ErrorManager::get_flash();
      ?>

      <?php if ( !empty($t) ) { ?>
      <h2>
      <?php echo $t; ?>
      </h2>
      <?php } ?>
