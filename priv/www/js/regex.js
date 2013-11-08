$(document).ready(function() {
  $('[data-behavior~=submit-regex-query]').keyup(function(e) {
    e.preventDefault;

    test_string = $('#test_string').val();
    regex = $('#regex').val();

    if (regex && test_string) {
      $.ajax("/regex?text="+test_string+"&pattern="+regex)
        .done(function(data) {
          $('#match_result').html(data);
        })
        .fail(function() {
          alert( "error" );
        })
    }
  });
});
