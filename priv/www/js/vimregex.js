$(document).ready(function() {
  $('[data-behavior~=submit-regex-query]').keypress(function(e) {
    e.preventDefault;
    $.ajax("/highlight")
      .done(function() {
        alert( "success" );
      })
      .fail(function() {
        alert( "error" );
      })
  });
});
