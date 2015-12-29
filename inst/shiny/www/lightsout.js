var lightsoutInit = function() {
  $('#board').on( 'click', '.board-cell', function() {
    var message = {
      reqid : Math.round(Math.random() * 10000000),
      col : $(this).data('col'),
      row : $(this).data('row')
    };
    Shiny.onInputChange('click', message);
  });
};

$(function() { lightsoutInit(); })();
