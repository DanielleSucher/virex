$(document).ready(function() {
  $('#update_regex').click(function(e) {
    e.preventDefault;

    test_string = $('#test_string').val();
    regex = $('#regex').val();

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
});
