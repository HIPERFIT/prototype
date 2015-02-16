function arrayToObj(xs) {
    var res = {}; 
    $.each(xs, function(i,x) { res[x.name]=x.value }); 
    return res
}

$(document).ready(function() {
    $('#startDatePicker, #endDatePicker')
        .datepicker({autoclose: true,
                     todayHighlight:true, 
                     format: 'yyyy-mm-dd'});
    $('#run').click(function() {
        $('#result').hide();
        $('#error').hide();
        var data = arrayToObj($('#mainForm').serializeArray());
        $.post('/api/', JSON.stringify(data))
            .done(function(resp) { $('#result').html(resp.price);
                                   $('#result').show();
                                 })
            .fail(function(jqXHR, textStatus, errorThrown) {
                $('#error').html(jqXHR.responseText);
                $('#error').show();
            })
    })
});
