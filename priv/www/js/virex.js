$(document).ready(function() {
  $('#update_regex').click(function(e) {
    e.preventDefault;

    test_string = encodeURIComponent($('#test_string').val());
    regex = encodeURIComponent($('#regex').val());

    if (regex && test_string) {
      $('#match_result').css('background-color', '#EBEBEB');
      $('#match_groups').css('background-color', '#EBEBEB');

      $.ajax("/regex?text="+test_string+"&pattern="+regex)
        .done(function(data) {
          $('#results').html(data);
          $('#match_result').css('background-color', 'white');
          $('#match_groups').css('background-color', 'white');
        });
    }
  });
  $("#regex").keyup( function() {
    regex = $('#regex').val();
    warning_div = $('#slash_warning')
    if (/(^|[^\\])(\\{2})*\//.test(regex)) {
      warning_div.html("&nbsp;&nbsp;WARNING: This regex contains an unescaped /");
    } else {
      warning_div.html("<br />");
    }
  });
});
