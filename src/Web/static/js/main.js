function arrayToObj(xs) {
    var res = {}; 
    $.each(xs, function(i,x) { res[x.name]=x.value }); 
    return res
}

function collectData (ins) {
    var res = {};
    var v;
    $.each($(ins), function (i, x) {
        if ($(x).data('datatype') === 'Double') { 
            v = parseFloat($(x).val());
        } else v = $(x).val();
        res[$(x).attr('name')] = v;
    });
    return res;
}

function populateSelect ($sel, opts) {
    $.each(opts, function (i, v) {
        console.log(v);
        $sel.append($("<option/>").val(v).text(v));
    });
    $sel.selectpicker("refresh");
}

$(document).ready(function() {
    $('.selectpicker').selectpicker();
    $('#startDatePicker, #endDatePicker')
        .datepicker({autoclose: true,
                     todayHighlight:true, 
                     format: 'yyyy-mm-dd'});
    $('#run').click(function() {
        $('#result').hide();
        $('#error').hide();
        var url = '/api/' + $('#mainForm').data("url");
        var data = collectData($('.form-control'));
        $.post(url, JSON.stringify(data))
            .done(function(resp) { $('#result').html(resp.price);
                                   $('#result').show();
                                 })
            .fail(function(jqXHR, textStatus, errorThrown) {
                $('#error').html(jqXHR.responseText);
                $('#error').show();
            })
    })
    $.get('/marketData/underlyings/', function (data) {populateSelect($("select[data-datatype='Underlying']"), data)});
});
