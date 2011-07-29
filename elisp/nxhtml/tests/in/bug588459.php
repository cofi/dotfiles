<?php

include_partial( 'global/proctorheader' );

$student = get('student');
$detail_questions = get("detail_questions");

$event = get("event");
$event_template = get("event_template");
$location = get("location");

$payment = get('payment');
$card_info = get('card_info');
$error_hash = get('form_errors');
$payment_required = get('payment_required');
$offline_payment = get('offline_payment');

$past_date_event = get('past_date_event');
$reg_allow_walk_ins = get('reg_allow_walk_ins');
$reg_require_cert_id = get('reg_require_cert_id');
$event_min_age = get('event_min_age');
$calculations_date = get('calculations_date');

$registration = get("registration");

$states = get("states");
$states_array = get("states_array");

$months = array("01" => "01 - January", "02" => "02 - February", "03" => "03 - March", "04" => "04 - April",
  "05" => "05 - May", "06" => "06 - June", "07" => "07 - July", "08" => "08 - August", "09" => "09 - September",
  "10" => "10 - October", "11" => "11 - November", "12" => "12 - December");

$req = "<span style=\"color: red\">*</span>";

$current_year = date("Y");
$forward_year = $current_year+10;

/// TODO: Replace these with config options (FB696)
$p_address_on = 1;
$p_address_type = 'Mailing';
$s_address_on = 1;
$s_address_type = 'Physical';

?>

<style type="text/css">
.validationText {
  font-size: 11px;
  font-weight: bold;
  color: #911;
}
</style>

<script type="text/javascript">
<!--
function enable_fields() {
    document.getElementById('registration_s_address1').disabled = false;
    if ( document.getElementById('registration_s_address1').value == "" ) {
      document.getElementById('registration_s_address1').value = document.getElementById('registration_p_address1').value;
    }

    document.getElementById('registration_s_address2').disabled = false;
    if ( document.getElementById('registration_s_address2').value == "" ) {
      document.getElementById('registration_s_address2').value = document.getElementById('registration_p_address2').value;
    }

    document.getElementById('registration_s_city').disabled = false;
    if ( document.getElementById('registration_s_city').value == "" ) {
      document.getElementById('registration_s_city').value = document.getElementById('registration_p_city').value;
    }

    document.getElementById('registration_s_state_cd').disabled = false;
    if ( document.getElementById('registration_s_state_cd').value == "" ) {
      document.getElementById('registration_s_state_cd').value = document.getElementById('registration_p_state_cd').value;
    }

    document.getElementById('registration_s_zip').disabled = false;
    if ( document.getElementById('registration_s_zip').value == "" ) {
      document.getElementById('registration_s_zip').value = document.getElementById('registration_p_zip').value;
    }

    document.getElementById('registration_s_phone').disabled = false;
    if ( document.getElementById('registration_s_phone').value == "" ) {
      document.getElementById('registration_s_phone').value = document.getElementById('registration_p_phone').value;
    }

    return true;
}

function handle_change() {
  if ( document.getElementById('s_addr_is_same').checked ) {
    document.getElementById('s_addr').style.display = 'block';
    if ( document.getElementById('registration_s_address1').value == "" ) {
      document.getElementById('registration_s_address1').value = document.getElementById('registration_p_address1').value;
    }
    if ( document.getElementById('registration_s_address2').value == "" ) {
      document.getElementById('registration_s_address2').value = document.getElementById('registration_p_address2').value;
    }
    if ( document.getElementById('registration_s_city').value == "" ) {
      document.getElementById('registration_s_city').value = document.getElementById('registration_p_city').value;
    }
    if ( document.getElementById('registration_s_state_cd').value == "" ) {
      document.getElementById('registration_s_state_cd').value = document.getElementById('registration_p_state_cd').value;
    }
    if ( document.getElementById('registration_s_zip').value == "" ) {
      document.getElementById('registration_s_zip').value = document.getElementById('registration_p_zip').value;
    }
    if ( document.getElementById('registration_s_phone').value == "" ) {
      document.getElementById('registration_s_phone').value = document.getElementById('registration_p_phone').value;
    }
  }
  else {
    document.getElementById('s_addr').style.display = "none";
  }
}

function update_cc() {
  if ( ($('cc_first_name') == null) || ($('cc_last_name') == null) ){
    return false;
  }
  if ( document.getElementById('cc_first_name').value == "" ) {
    document.getElementById('cc_first_name').value = document.getElementById('student_first_name').value;
  }
  if ( document.getElementById('cc_last_name').value == "" ) {
    document.getElementById('cc_last_name').value = document.getElementById('student_last_name').value;
  }
}
//-->
</script>

<p>
This will create a student and register him for the <?php echo $event_template->attrib('event_name'); ?>  on
 <?php echo $event->attrib('event_datetime'); ?> at <?php echo $location->attrib('location_name'); ?>.<br/>
 Please fill out the required fields below and click "Create New Registration."
</p>

<form method="post" id="student_edit_form" action="" onsubmit="return enable_fields();" onsubmit="return k.korrect();">
<input type="hidden" name="Student[student_id]" value="<?php echo $student->attrib($student->id_field); ?>" />
<input type="hidden" name="Registration[registration_id]" value="<?php echo $registration->attrib('registration_id'); ?>" />
<input type="hidden" name="Registration[event_id]" id="student_event_id" value="<?php echo $event->attrib('event_id'); ?>" />
<input type="hidden" name="Registration[event_template_id]" id="student_event_template_id" value="<?php echo $event->attrib('event_template_id'); ?>" />
<input type="hidden" name="event_id" value="<?php echo $event->attrib('event_id'); ?>" id="event_id" />

<h3>Student Information</h3>

<table width="800" cellspacing="5" cellpadding="0" border="0">
  <tbody>
    <tr>
      <td width="200"><label for="student_first_name">First Name:</label></td>
      <td><input type="text" name="Student[first_name]" value="<?php echo $student->attrib('first_name'); ?>" id="student_first_name" class="form_text" style="width:98%;_width:98%;" onchange="update_cc();update_acct_info();" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="student_middle_initial">Middle Initial:</label></td>
      <td><input type="text" name="Student[middle_initial]" value="<?php echo $student->attrib('middle_initial'); ?>" id="student_middle_initial" class="form_text" style="width:10%;_width:10%;" onchange="update_cc();update_acct_info();" /></td>
    </tr>

    <tr>
      <td><label for="student_last_name">Last Name:</label></td>
      <td><input type="text" name="Student[last_name]" value="<?php echo $student->attrib('last_name'); ?>" id="student_last_name" class="form_text" style="width:98%;_width:98%;" onchange="update_cc();update_acct_info();" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="student_suffix">Suffix:</label></td>
      <td>
      <?= generic_select_field("Student[suffix]", array("class" => 'form_text', "id" => 'student_suffix'), name_suffixes(), $student->attrib('suffix'), ' ') ?>
      </td>
    </tr>

    <tr>
      <td><label for="student_email">Email:</label></td>
      <td><input type="text" name="Student[email]" value="<?php echo $student->attrib('email'); ?>" id="student_email" class="form_text" style="width:98%;_width:98%;" /></td>
      <td></td>
    </tr>

    <tr>
      <td><label for="student_dob">Date of Birth:</label></td>
      <td>
        <?php echo month_select_field( 'StudentBD[dob_month]', 'id="dob_month" class="form_text"', $student->attrib('dob') ); ?>
        <?php echo mday_select_field( 'StudentBD[dob_day]', 'id="dob_day" class="form_text"' , $student->attrib('dob') ); ?>
        <?php echo year_select_field( 'StudentBD[dob_year]', 'id="dob_year" class="form_text"', $student->attrib('dob') ); ?>
      </td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="student_gender">Gender:</label></td>
      <td>
        <select name="Student[gender]" id="student_gender" class="form_text">
          <option value=""></option>
          <option value="M" <?php echo ( $student->attrib('gender') == 'M' ? "selected" : "" ); ?>>Male</option>
          <option value="F" <?php echo ( $student->attrib('gender') == 'F' ? "selected" : "" ); ?>>Female</option>
        </select>
      </td>
      <td><span class='flag_required'>*</span></td>
    </tr>
  </tbody>
</table>


<h3>Registration Information</h3>

<?php if ( $past_date_event ) { ?>
<table width="800" cellspacing="5" cellpadding="0" border="0">
  <tbody>
    <tr>
      <td width="200"><label for="walk_in_student">Walk-in student?</label></td>
      <td>
        <select name="walk_in_student" id="walk_in_student" class="form_text">
          <option value="1" selected="selected">Yes</option>
          <option value="0">No</option>
        </select>
        <span class='flag_required'>*</span>
      </td>
      <td></td>
    </tr>
  </tbody>
</table>
<?php } ?>

<?php if ( $reg_require_cert_id ) { ?>
<table width="800" cellspacing="5" cellpadding="0" border="0">
  <tbody>
    <tr>
      <td width="200"><label for="reg_cert_id">Certificate ID:</label></td>
      <td>
      <input type="text" name="reg_cert_id" value="Enter a valid certificate ID (Required)" id="walk_in_cert_id" class="form_text" style="color:gray;width:98%;_width:98%;" onblur="if(this.value=='')this.value='Enter a valid certificate ID (Required)',this.style.color='gray';" onfocus="if(this.value=='Enter a valid certificate ID (Required)')this.value='';this.style.color='black';" />
      </td>
      <td><span class='flag_required'>*</span></td>
    </tr>
  </tbody>
</table>
<?php } ?>


<table width="800" cellspacing="5" cellpadding="0" border="0">
  <tbody>
    <tr>
      <td width="200"><label for="registration_p_address1"><?php echo $p_address_type;?> Address:</label></td>
      <td><input type="text" name="Registration[p_address1]" value="<?php echo $registration->attrib('p_address1'); ?>" id="registration_p_address1" class="form_text" style="width:98%;_width:98%;" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="registration_p_address2"><?php echo $p_address_type;?> Address 2:</label></td>
      <td><input type="text" name="Registration[p_address2]" value="<?php echo $registration->attrib('p_address2'); ?>" id="registration_p_address2" class="form_text" style="width:98%;_width:98%;" /></td>
    </tr>

    <tr>
      <td><label for="registration_p_city"><?php echo $p_address_type;?> City:</label></td>
      <td><input type="text" name="Registration[p_city]" value="<?php echo $registration->attrib('p_city'); ?>" id="registration_p_city" class="form_text" style="width:98%;_width:98%;" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="registration_p_state_cd"><?php echo $p_address_type;?> State:</label></td>
      <td>
      <select name='Registration[p_state_cd]' id="registration_p_state_cd" class="form_text">
      <?php
        foreach( $states as $state ) {
          echo "<option value=\"" . $state->attrib($state->id_field) . "\"" . ( ( $state->attrib($state->id_field) == $location->attrib('state_cd') ) ? " selected=\"selected\"" : "" ) . ">" . $state->attrib('state_name') . "</option>\n";
        }
      ?>
      </select>
      </td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="registration_p_zip"><?php echo $p_address_type;?> Zip:</label></td>
      <td><input type="text" name="Registration[p_zip]" value="<?php echo $registration->attrib('p_zip'); ?>" id="registration_p_zip" class="form_text" style="width:98%;_width:98%;" maxlength="10" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="registration_p_phone">Phone:</label></td>
      <td><input type="text" name="Registration[p_phone]" value="<?php echo $registration->attrib('p_phone'); ?>" id="registration_p_phone" class="form_text" style="width:98%;_width:98%;" maxlength="20" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td></td>
      <td><input type="checkbox" id="s_addr_is_same" onclick="handle_change();" />
          <label for="s_addr_is_same">Click here if <?php echo strtolower($s_address_type); ?> address is different.</label>
      </td>
    </tr>
  </table>

  <table width="800" cellspacing="5" cellpadding="0" border="0" id="s_addr">
    <tr>
      <td width="200"><label for="registration_s_address1"><?php echo $s_address_type;?> Address:</label></td>
      <td width="600"><input type="text" name="Registration[s_address1]" value="<?php echo $registration->attrib('s_address1'); ?>" id="registration_s_address1" class="form_text" style="width:98%;_width:98%;" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="registration_s_address2"><?php echo $s_address_type;?> Address 2:</label></td>
      <td><input type="text" name="Registration[s_address2]" value="<?php echo $registration->attrib('s_address2'); ?>" id="registration_s_address2" class="form_text" style="width:98%;_width:98%;" /></td>
    </tr>

    <tr>
      <td><label for="registration_s_city"><?php echo $s_address_type;?> City:</label></td>
      <td><input type="text" name="Registration[s_city]" value="<?php echo $registration->attrib('s_city'); ?>" id="registration_s_city" class="form_text" style="width:98%;_width:98%;" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="registration_s_state_cd"><?php echo $s_address_type;?> State:</label></td>
      <td>
      <select name='Registration[s_state_cd]' id="registration_s_state_cd" class="form_text">
      <?php
        foreach( $states as $state ) {
          echo "<option value=\"" . $state->attrib($state->id_field) . "\"" . ( ( $state->attrib($state->id_field) == $location->attrib('state_cd') ) ? " selected=\"selected\"" : "" ) . ">" . $state->attrib('state_name') . "</option>\n";
        }
      ?>
      </select>
      </td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="registration_s_zip"><?php echo $s_address_type;?> Zip:</label></td>
      <td><input type="text" name="Registration[s_zip]" value="<?php echo $registration->attrib('s_zip'); ?>" id="registration_s_zip" class="form_text" style="width:98%;_width:98%;" maxlength="10" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="registration_s_phone">Phone:</label></td>
      <td><input type="text" name="Registration[s_phone]" value="<?php echo $registration->attrib('s_phone'); ?>" id="registration_s_phone" class="form_text" style="width:98%;_width:98%;" maxlength="20" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>
  </table>

<?php if ( $detail_questions ) { ?>
  <h3>Additional Information</h3>

  <table width="800" cellspacing="5" cellpadding="0" border="0">

<?php
$student_detail_validations = "";
foreach( $detail_questions as $detail_question ) {
  $detail_validation = "";

  // show questions with fixed values as html select (must start with open brace!)
  if ( ( strpos($detail_question['validation'], "|") !== false ) &&
       ( substr( $detail_question['validation'], 0, 1 ) == "{" ) ) {
    $html_select_values = explode ( "|", trim ($detail_question['validation'], "{}") );
    if (!empty($html_select_values)) {
      $detail_question['display_as'] = 'html_select';
      $detail_question['html_select_values'] = $html_select_values;
    }
  }
  // Create the JSON for Korrector
  if ( isset( $detail_question['validation'] )
       && strpos( $detail_question['validation'], '{' ) !== 0 ) {
    $student_detail_validations .= ", addl_"
      . $detail_question['detail_question_id']
      . ": [ { "
      . "regex: '"
      . addslashes($detail_question['validation'])
      . "', "
      . "errorText: 'Invalid format.' } ]";
  } else if ( isset( $detail_question['validation'] ) ) {
    if ( $detail_question['is_required'] == '1' ) {
      $detail_validation =
        preg_replace('/^\{\s*\|(.*)\}$/', '(\1)', $detail_question['validation']);
    } else {
      $detail_validation =
        preg_replace('/^\{(.*)\}$/', '(\1)', $detail_question['validation']);
    }
    $student_detail_validations .= ", addl_"
      . $detail_question['detail_question_id']
      . ": [ { "
      . "regex: '"
      . addslashes( $detail_validation )
      . "', "
      . "errorText: 'Invalid value.' } ]";
  }

  if ( $detail_question['display_as'] == 'html_select' ) {
?>

    <tr>
      <td width="200" valign='top'><label for="addl_<?php echo $detail_question['question_key']; ?>"><?php echo $detail_question['question_name']; ?></label></td>
      <td valign='top'>
      <select name="addl[<?php echo $detail_question['question_key']; ?>]" id="addl_<?php echo $detail_question['detail_question_id']; ?>" class="form_text">
        <?php foreach ($detail_question['html_select_values'] as $option) { ?>
        <option value="<?php echo $option; ?>" <?php echo ( $option == $detail_question['question_answer'] ? "selected" : "" ); ?>><?php echo $option; ?></option>
        <?php } ?>
      </select>
      </td>
      <td>
      <?php if ( $detail_question['is_required'] ) { ?>
        <span class='flag_required'>*</span>
      <?php } ?>
      </td>
    </tr>

<?php
  } else {
?>

    <tr>
      <td width="200" valign='top'><label for="addl_<?php echo $detail_question['question_key']; ?>"><?php echo $detail_question['question_name']; ?></label></td>
      <td valign='top'>
      <input type="text" name="addl[<?php echo $detail_question['question_key']; ?>]" id="addl_<?php echo $detail_question['detail_question_id']; ?>" value="<?php echo $detail_question['question_answer']; ?>" class="form_text" style="width:98%;_width:98%;" />
      <?php if ( $detail_question['assigned_value'] ) { ?>
      <div>
      <?php echo ( $detail_question['assigned_value_desc'] ? $detail_question['assigned_value_desc'] : $detail_question['question_desc'] ); ?>
      </div>
      <?php } ?>
      </td>
      <td>
      <?php if ( $detail_question['is_required'] ) { ?>
        <span class='flag_required'>*</span>
      <?php } ?>
      </td>
    </tr>

<?php
  }
}
}
?>

  </tbody>
</table>


<h3>Account Information</h3>

<table width="800" cellspacing="5" cellpadding="0" border="0">
  <tbody>
    <tr>
      <td width="200"><label for="student_uname">Username:</label></td>
      <td><input type="text" name="Student[uname]" value="<?php echo $student->attrib('uname'); ?>" id="student_uname" class="form_text unamecheck" style="width:98%;_width:98%;" onchange="clear_acct_info();" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="student_password">Password:</label></td>
      <td><input type="text" value="" name="Student[password]" id="student_password" class="form_text" style="width:98%;_width:98%;" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="password_confirm">Confirm Password:</label></td>
      <td><input type="text" value="" name="password_confirm" id="password_confirm" class="form_text" style="width:98%;_width:98%;" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>
  </tbody>
</table>

<table width="800" cellspacing="5" cellpadding="0" border="0" style="margin-top:20px;">
  <tbody>
    <tr>
      <td width="200"><label for="student_password_hint">Password Hint:</label></td>
      <td><input type="text" name="Student[password_hint]" value="<?php echo $student->attrib('password_hint'); ?>" id="student_password_hint" class="form_text" style="width:98%;_width:98%;" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="student_secret_question">Secret Question:</label></td>
      <td><input type="text" name="Student[secret_question]" value="<?php echo $student->attrib('secret_question'); ?>" id="student_secret_question" class="form_text" style="width:98%;_width:98%;" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>

    <tr>
      <td><label for="student_secret_answer">Secret Answer:</label></td>
      <td><input type="text" name="Student[secret_answer]" value="<?php echo $student->attrib('secret_answer'); ?>" id="student_secret_answer" class="form_text" style="width:98%;_width:98%;" /></td>
      <td><span class='flag_required'>*</span></td>
    </tr>
  </tbody>
</table>


<?php if ( $payment_required ) { ?>
<h3>Payment Information</h3>

<div style="font-size:120%;padding:0 10px 5px 0;">
<input type="checkbox" name="offline_payment" id="offline_payment" onclick="if(this.checked) { document.getElementById('payment_details').style.display='none'; } else { document.getElementById('payment_details').style.display='block'; }" <?= ($offline_payment) ? 'checked' : '' ?> />
<strong>Skip Online Payment Collection?</strong>
(i.e. walk-in students where payment has already been collected)
</div>

<div id="payment_details" <?= ($offline_payment) ? 'style="display:none;"' : '' ?>>
<table width="800" cellspacing="5" cellpadding="0" border="0">
  <tbody>
    <tr>
      <td width="200"><label for="cc_first_name">Cardholder First Name:</label></td>
      <td><?= generic_text_field("cc_first_name", array("id" => 'cc_first_name', "class" => 'form_text', "size" => "25", "maxlength" => 45 ), $card_info->cc_first_name) ?><?= $req ?> <?= get_model_error("cc_first_name", $card_info) ?></td>
    </tr>

    <tr>
      <td><label for="cc_last_name">Cardholder Last Name:</label></td>
      <td><?= generic_text_field("cc_last_name", array("id" => 'cc_last_name', "class" => 'form_text', "size" => "25", "maxlength" => 45 ), $card_info->cc_last_name) ?><?= $req ?> <?= get_model_error("cc_last_name", $card_info) ?></td>
    </tr>

    <tr>
      <td><label for="cc_type">Credit Card Type:</label></td>
                                 <td><?= generic_select_field("cc_type", array("id" => 'cc_type', "class" => 'form_text'), array('American Express','Mastercard','Visa','Discover'), $card_info->cc_type) ?><?= $req ?> <?= get_model_error("cc_type", $card_info) ?></td>
    </tr>

    <tr>
      <td><label for="cc_number">Credit Card Number:</label></td>
                                   <td><?= generic_text_field("cc_number", array("id" => 'cc_number', "class" => 'form_text', "size" => "25", "maxlength" => 25 ), $card_info->cc_number) ?><?= $req ?> <?= get_model_error("cc_process", $card_info) ?><?= get_model_error("cc_number", $card_info) ?></td>
    </tr>

    <tr>
      <td><label for="cc_exp_mon">Credit Card Expiration:</label></td>
      <td><?= generic_select_field("cc_exp_mon", array("id" => 'cc_exp_mon', "class" => 'form_text'), $months, $card_info->cc_exp_mon, "Month") ?> <?= $req ?> / <?= generic_select_field("cc_exp_yr", array("id" => 'cc_exp_yr', "class" => 'form_text'), range($current_year, $forward_year), $card_info->cc_exp_yr,"Year") ?><?= $req ?></td>
    </tr>

    <?php if($card_info->errors['cc_exp_mon'] or $card_info->errors['cc_exp_yr']) { ?>
      <tr>
        <td></td>
        <td><?= get_model_error("cc_exp_mon", $card_info) ?>
        <?= get_model_error("cc_exp_yr", $card_info) ?></td>
      </tr>
    <?php } ?>

    <tr>
      <td></td>
      <td>
      <input type="checkbox" name="billing_different" id="billing_different" onclick="if(this.checked && ($('bill_addr') != null)) { document.getElementById('bill_addr').style.display='block'; } else { document.getElementById('bill_addr').style.display='none'; }" <?= (request('billing_different') == 'on') ? 'checked' : '' ?> />
      <label for="billing_different">Check here if the billing address is different from the mailing address.</label>
      </td>
    </tr>
  </tbody>
</table>

<div id="bill_addr">
<table width="800" cellspacing="5" cellpadding="0" border="0">
<tbody>
  <tr>
    <td width="200"><label for="b_address1">Billing Address 1:</label></td>
    <td><?= model_text_field($payment, "b_address1", array("id" => 'b_address1', "class" => 'form_text', "size" => "40", "maxlength" => 45)) ?><?= $req ?> <?= get_model_error("b_address1", $payment) ?></td>
  </tr>

  <tr>
    <td><label for="b_address2">Billing Address 2:</label></td>
    <td><?= model_text_field($payment, "b_address2", array("id" => 'b_address2', "class" => 'form_text', "size" => "40", "maxlength" => 45 )) ?></td>
  </tr>

  <tr>
    <td><label for="b_city">City:</label></td>
    <td><?= model_text_field($payment, "b_city", array("id" => 'b_city', "class" => 'form_text', "size" => "40", "maxlength" => 45 )) ?><?= $req ?> <?= get_model_error("b_city", $payment) ?></td>
  </tr>

  <tr>
    <td><label for="b_state_cd">State:</label></td>
    <td><?= model_select_field($payment, "b_state_cd", array("id" => 'b_state_cd', "class" => 'form_text'), $states_array) ?><?= $req ?> <?= get_model_error("b_state_cd", $payment) ?></td>
  </tr>

  <tr>
    <td><label for="b_zip">Zip:</label></td>
    <td><?= model_text_field($payment, "b_zip", array("id" => 'b_zip', "class" => 'form_text', "size" => "10", "maxlength" => "10")) ?><?= $req ?> <?= get_model_error("b_zip", $payment) ?></td>
  </tr>

  <tr>
    <td><label for="b_phone">Phone:</label></td>
    <td><?= model_text_field($payment, "b_phone", array("id" => 'b_phone', "class" => 'form_text', "size" => "20", "maxlength" => "20")) ?><?= $req ?> <i>(e.g. 888-555-1212)</i><?= get_model_error("b_phone", $payment) ?></td>
  </tr>
</tbody>
</table>
</div>
</div>

<? } // payment_required ?>

<h3>Notifications</h3>

  <table width="800" cellspacing="5" cellpadding="0" border="0">

    <tr>
      <td width="200"><label for="notify_student">Notify student via Email?</label></td>
      <td>
        <select name="notify_student" id="notify_student" class="form_text">
          <option value="1" selected="selected">Yes</option>
          <option value="0">No</option>
        </select>
      </td>
    </tr>

  </tbody>
</table>

<hr/>

<div class="clear" style="margin-bottom: 26px; margin-top: 10px;">
  <a class="button" href="#" onclick="this.blur(); enable_fields(); if ( k.korrect() ) { document.getElementById('student_edit_form').submit(); } return false;"><span>Create New Registration</span></a>
  <a class="button" href="#" onclick="this.blur(); history.go(-1); return false;"><span>Cancel</span></a>
</div>

<div style="clear:both;"><br/></div>

<input type="hidden" name="unameresult" id="unameresult" value="false" />

<script type="text/javascript">
<!--
  var k = new Korrector( {
      student_uname: [ {
          regex: '^.+$',
          errorText: 'Username is a required field.'
        }, {
          regex: '<?php echo str_replace('\\', '\\\\\\', $student->validations['uname']['regex']); ?>',
          errorText: 'Username can contain only lowercase letters and numbers or a valid e-mail address.'
        }, {
          customValidation:
          function () {
            var result = document.getElementById('unameresult').value;
            // Reverse the boolean for validation
            result = (result == 'false');
            return result;
          },
          errorText: 'This username is already in use.'
        }
      ],
      student_first_name: [ {
          regex: '<?php echo $student->validations['first_name']['regex']; ?>',
          errorText: '<?php echo $student->validations['first_name']['error']; ?>'
        }
      ],
      student_middle_initial: [ {
        regex: '<?php echo $student->validations['middle_initial']['regex']; ?>',
        errorText: '<?php echo $student->validations['middle_initial']['error']; ?>'
        }
      ],
      student_last_name: [ {
          regex: '<?php echo $student->validations['last_name']['regex']; ?>',
          errorText: '<?php echo $student->validations['last_name']['error']; ?>'
        }
      ],
      student_suffix: [ {
          regex: '<?php echo $student->validations['suffix']['regex']; ?>',
          errorText: '<?php echo $student->validations['suffix']['error']; ?>'
        }
      ],
      student_gender: [ {
          regex: '^.+$',
          errorText: 'Gender is a required field.'
        }
      ],
      student_password: [ {
          regex: '^.+$',
          errorText: 'Password is a required field.'
        }
      ],
      password_confirm: [ {
          regex: '^.+$',
          errorText: 'Password Confirm is a required field.'
        }, {
          customValidation:
            function ( ) {
              return document.getElementById('student_password').value == document.getElementById('password_confirm').value;
            },
          errorText: 'Password and confirmation must match.'
        }
      ],
      student_email: [ {
        regex: '^$|<?php echo str_replace('\\', '\\\\\\', $student->validations['email']['regex']); ?>',
        errorText: '<?php echo str_replace('\\', '\\\\\\', $student->validations['email']['error']); ?>'
        }
      ],
      dob_month: [ {
          regex: '^[0-9]{2}$',
          errorText: 'Please select a Month for Date of Birth'
        }
      ],
      dob_day: [ {
          regex: '^[0-9]{2}$',
          errorText: 'Please select a Day for Date of Birth'
        }
      ],
      dob_year: [ {
          regex: '^[0-9]{4}$',
          errorText: 'Please select a Year for Date of Birth'
        }
      <?php if ( $event_min_age ) { ?>
        , {
          customValidation:
            function ( ) {
              if(document.getElementById('dob_month').value &&
                 document.getElementById('dob_day').value &&
                 document.getElementById('dob_year').value ) {
                var event_d = <?= date( "j", strtotime( $calculations_date))?>;
                var event_m = <?= date( "n", strtotime( $calculations_date))?>;
                var event_y = <?= date( "Y", strtotime( $calculations_date))?>;
                var dob_d = document.getElementById('dob_day').value;
                var dob_m = document.getElementById('dob_month').value;
                var dob_y = document.getElementById('dob_year').value;
                var min_age = <?=$event_min_age ?>;

                var year_diff = event_y - dob_y;
                var month_diff = event_m - dob_m;
                var day_diff = event_d - dob_d;

                if (month_diff < 0){
                  year_diff--;

                } else if ((month_diff == 0) && (day_diff < 0)) {
                  year_diff--;
                }

                return year_diff >=  min_age;
              }
            },
          errorText: 'You must be at least <?=$event_min_age ?> years old to register.'
        }
      <?php } ?>
      ],
      student_password_hint: [ {
          regex: '^.+$',
          errorText: 'Password hint is a required field.'
        }, {
          regex: '<?php echo $student->validations['password_hint']['regex']; ?>',
          errorText: '<?php echo $student->validations['password_hint']['error']; ?>'
        }
      ],
      student_secret_question: [ {
          regex: '^.+$',
          errorText: 'Secret question is a required field.'
        }, {
          regex: '<?php echo $student->validations['secret_question']['regex']; ?>',
          errorText: '<?php echo $student->validations['secret_question']['error']; ?>'
        }
      ],
      student_secret_answer: [ {
          regex: '^.+$',
          errorText: 'Secret answer is a required field.'
        }, {
          regex: '<?php echo $student->validations['secret_answer']['regex']; ?>',
          errorText: '<?php echo $student->validations['secret_answer']['error']; ?>'
        }
      ],
      registration_p_address1: [ {
          regex: '^.+$',
          errorText: 'Address is a required field.'
        }, {
          regex: '<?php echo $registration->validations['p_address1']['regex']; ?>',
          errorText: '<?php echo $registration->validations['p_address1']['error']; ?>'
        }
      ],
      registration_p_address2: [ {
          regex: '<?php echo $registration->validations['p_address2']['regex']; ?>',
          errorText: '<?php echo $registration->validations['p_address2']['error']; ?>'
        }
      ],
      registration_p_city: [ {
          regex: '^.+$',
          errorText: 'City is a required field.'
        }, {
          regex: '<?php echo $registration->validations['p_city']['regex']; ?>',
          errorText: '<?php echo $registration->validations['p_city']['error']; ?>'
        }
      ],
      registration_p_state_cd: [ {
          regex: '^.+$',
          errorText: 'State is a required field.'
        }, {
          regex: '<?php echo $registration->validations['p_state_cd']['regex']; ?>',
          errorText: '<?php echo $registration->validations['p_state_cd']['error']; ?>'
        }
      ],
      registration_p_phone: [ {
          regex: '^.+$',
          errorText: 'Phone number is a required field.'
        }, {
          regex: '<?php echo $registration->validations['p_phone']['regex']; ?>',
          errorText: '<?php echo $registration->validations['p_phone']['error']; ?>'
        }
      ],
      registration_p_zip: [ {
          regex: '^.+$',
          errorText: 'Zip is a required field.'
        }, {
          regex: '<?php echo $registration->validations['p_zip']['regex']; ?>',
          errorText: '<?php echo $registration->validations['p_zip']['error']; ?>'
        }
      ],
      registration_s_address1: [ {
           customValidation:
             function ( ) {
               return address_validation('s_addr_is_same', 'registration_s_address1',
               '<?php echo $registration->validations['s_address1']['regex']; ?>');
             },
           errorText: '<?php echo $registration->validations['s_address1']['error']; ?>'
         }
      ],
      registration_s_address2: [ {
          customValidation:
            function ( ) {
              return address_validation('s_addr_is_same', 'registration_s_address2',
                                        '<?php echo $registration->validations['s_address2']['regex']; ?>');
            },
          errorText: '<?php echo $registration->validations['s_address2']['error']; ?>'
        }
      ],
      registration_s_city: [ {
          customValidation:
            function ( ) {
              return address_validation('s_addr_is_same', 'registration_s_city',
                                        '<?php echo $registration->validations['s_city']['regex']; ?>');
            },
          errorText: '<?php echo $registration->validations['s_city']['error']; ?>'
        }
      ],
      registration_s_state_cd: [ {
          customValidation:
            function ( ) {
              return address_validation('s_addr_is_same', 'registration_s_state_cd',
                                        '<?php echo $registration->validations['s_state_cd']['regex']; ?>');
            },
          errorText: '<?php echo $registration->validations['s_state_cd']['error']; ?>'
        }
      ],
      registration_s_zip: [ {
          customValidation:
            function ( ) {
              return address_validation('s_addr_is_same', 'registration_s_zip',
                                        '<?php echo $registration->validations['s_zip']['regex']; ?>');
            },
          errorText: '<?php echo $registration->validations['s_zip']['error']; ?>'
        }
      ],
      registration_s_phone: [ {
          customValidation:
            function ( ) {
              return address_validation('s_addr_is_same', 'registration_s_phone',
                                        '<?php echo $registration->validations['s_phone']['regex']; ?>');
            },
          errorText: '<?php echo $registration->validations['s_phone']['error']; ?>'
        }
      ]
<?php if ( $payment_required ) { ?>
        , b_address1: [ {
          customValidation:
            function ( ) {
              return address_validation('billing_different', 'b_address1',
                                        '<?php echo $payment->validations['b_address1']['regex']; ?>');
            },
          errorText: '<?php echo $payment->validations['b_address1']['error']; ?>'
        }
      ],
      b_address2: [ {
          customValidation:
            function ( ) {
              return address_validation('billing_different', 'b_address2',
                                        '<?php echo $payment->validations['b_address2']['regex']; ?>');
            },
          errorText: '<?php echo $payment->validations['b_address2']['error']; ?>'
        }
      ],
      b_city: [ {
          customValidation:
            function ( ) {
              return address_validation('billing_different', 'b_city',
                                        '<?php echo $payment->validations['b_city']['regex']; ?>');
            },
          errorText: '<?php echo $payment->validations['b_city']['error']; ?>'
        }
      ],
      b_state_cd: [ {
          customValidation:
            function ( ) {
              return address_validation('billing_different', 'b_state_cd',
                                        '<?php echo $payment->validations['b_state_cd']['regex']; ?>');
            },
          errorText: '<?php echo $payment->validations['b_state_cd']['error']; ?>'
        }
      ],
      b_zip: [ {
          customValidation:
            function ( ) {
              return address_validation('billing_different', 'b_zip',
                                        '<?php echo $payment->validations['b_zip']['regex']; ?>');
            },
          errorText: '<?php echo $payment->validations['b_zip']['error']; ?>'
        }
      ],
      b_phone: [ {
          customValidation:
            function ( ) {
              return address_validation('billing_different', 'b_phone',
                                        '<?php echo $payment->validations['b_phone']['regex']; ?>');
            },
          errorText: '<?php echo $payment->validations['b_phone']['error']; ?>'
        }
      ],
      cc_first_name: [ {
          customValidation:
            function ( ) {
              return offline_payment_validation('cc_first_name',
                                        '<?php echo $payment->validations['card_first_name']['regex']; ?>');
            },
          errorText: '<?php echo $payment->validations['card_first_name']['error']; ?>'
        }
      ],
      cc_last_name: [ {
          customValidation:
            function ( ) {
              return offline_payment_validation('cc_last_name',
                                        '<?php echo $payment->validations['card_last_name']['regex']; ?>');
            },
          errorText: '<?php echo $payment->validations['card_last_name']['error']; ?>'
        }
      ],
      cc_type: [ {
          customValidation:
            function ( ) {
              return offline_payment_validation('cc_type', '^.+$');
            },
          errorText: 'Credit Card Type is a required field.'
        }
      ],
      cc_number: [ {
          customValidation:
            function ( ) {
              return offline_payment_validation('cc_number', '^.+$');
            },
          errorText: 'Credit Card Number is a required field.'
        }
      ],
      cc_exp_mon: [ {
          customValidation:
            function ( ) {
              return offline_payment_validation('cc_exp_mon', '^.+$');
            },
          errorText: 'Expiration Month is a required field.'
        }
      ],
          cc_exp_yr: [ {
          customValidation:
            function ( ) {
              return offline_payment_validation('cc_exp_yr', '^.+$');
            },
          errorText: 'Expiration Year is a required field.'
        }
      ]

<?php }
      echo $student_detail_validations;
 ?>
    }
  );

  handle_change();

if ($('bill_addr') != null) {
  if (document.getElementById('bill_addr').checked) {
    document.getElementById('bill_addr').style.display='block';
  } else {
    document.getElementById('bill_addr').style.display='none';
  }
}

function offline_payment_validation (fieldName, regexp) {
  if ( document.getElementById('offline_payment').checked ) {
    // skip validations
    return true;
  } else {
    pattern = new RegExp( regexp, 'g' );
    if ( document.getElementById(fieldName) ) {
      return pattern.test( document.getElementById(fieldName).value );
    }
  }
}

function address_validation (flagName, fieldName, regexp) {
  if ( document.getElementById(flagName).checked ) {
    pattern = new RegExp( regexp, 'g' );
    if ( document.getElementById(fieldName) ) {
      return pattern.test( document.getElementById(fieldName).value );
    }
  }
  return true;
}

function update_acct_info() {
  var curr_uname = document.getElementById('student_uname').value;
  var first_name = document.getElementById('student_first_name').value;
  var middle_initial = document.getElementById('student_middle_initial').value;
  var last_name = document.getElementById('student_last_name').value;

  if ((first_name.length < 2) || (last_name.length < 2)) {
    return false;
  }

  uname = first_name.substr(0,1);
  if (middle_initial.length > 0) {
    uname = uname + middle_initial.substr(0,1);
  }
  uname = uname + last_name.replace(/\'/,'');
  uname = uname.toLowerCase();
  uname = uname + Math.ceil(Math.random()*10000);

  document.getElementById('student_uname').value = uname;
  document.getElementById('student_password').value = uname;
  document.getElementById('password_confirm').value = uname;
  document.getElementById('student_password_hint').value = 'Same as your username';
  document.getElementById('student_secret_question').value = 'What is your username';
  document.getElementById('student_secret_answer').value = uname;
  return true;
}
jQuery('.unamecheck').bind('blur', checkUname);
function clear_acct_info() {
  document.getElementById('student_password').value = '';
  document.getElementById('password_confirm').value = '';
  document.getElementById('student_password_hint').value = '';
  document.getElementById('student_secret_question').value = '';
  document.getElementById('student_secret_answer').value = '';
  return true;
}
//-->
</script>

<script type="text/javascript" src="<?php echo AccessHelper::get_path(); ?>/js/jquery.auto-uname.js"></script>
<?php
include_partial( 'global/proctorfooter' );
?>
