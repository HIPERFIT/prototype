function collectData (ins) {
    var res = {};
    var v;
    $.each(ins, function (i, x) {
        if ($(x).data('datatype') === 'Double') { 
            v = parseFloat($(x).val());
        } else v = $(x).val();
        res[$(x).attr('name')] = v;
    });
    return res;
}

function populateSelect ($sel, opts) {
    $.each(opts, function (i, v) {
        $sel.append($("<option/>").val(v).text(v));
    });
    $sel.selectpicker("refresh");
}

function displayPrices ($cells, prices) {
    $cells.each(function(i,v) { 
        $(v).html(prices[i])
    });
}

$(document).ready(function() {
    $('.selectpicker').selectpicker();
    $('#startDatePicker, #endDatePicker')
        .datepicker({autoclose: true,
                     todayHighlight:true, 
                     format: 'yyyy-mm-dd'});
    $('#add').click(function() {
        $('#result').empty().hide();
        $('#error').empty().hide();
        var url = '/api/' + $('#mainForm').data("url");
        var contract = collectData($('.contract-data .form-control'));
        var common = collectData($('.common-data .form-control'));
        $.post(url, { "contractData" : JSON.stringify(contract),
                      "common" : JSON.stringify(common)
                    })
            .done(function(resp) { $('#result').html(resp.msg);
                                   $('#result').show();
                                 })
            .fail(function(jqXHR, textStatus, errorThrown) {
                $('#error').html(jqXHR.responseText);
                $('#error').show();
            })
    })
    $('#run').click(function() {
        var data = collectData($('.form-control'));
        var url = '/pricer/';
        $.post(url, { 'conf' : JSON.stringify(data) })
            .done(function(resp) { 
                displayPrices($('.price-output'), resp.prices);
                $('.total-output').html(resp.total);
            })
            .fail(function(jqXHR, textStatus, errorThrown) {
                $('#error').html(jqXHR.responseText);
                $('#error').show();
            })
    });
    $.get('/marketData/underlyings/', function (data) {populateSelect($("select[data-datatype='Underlying']"), data)});
    $('.del-item').click(function() {
        $.ajax({
            type: 'DELETE',
            url: '/portfolio/' + $(this).data('id')
        })
        .done(function() {
            location.reload();
        });
    })
});
