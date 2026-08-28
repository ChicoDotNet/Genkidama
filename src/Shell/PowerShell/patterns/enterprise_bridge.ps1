Set-StrictMode -Version Latest
# Bridge: abstraction delegates to a replaceable implementation.
$sender={param($text)"sms:$text"};$notify={param($text)&$script:sender $text};if((&$notify 'ok')-ne'sms:ok'){throw 'Bridge failed'}
