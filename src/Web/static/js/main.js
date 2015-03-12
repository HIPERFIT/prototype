function collectData (ins) {
    var res = {};
    var v;
    $.each(ins, function (i, x) {
        var $x = $(x);
        if ($x.val() !== "") {
            if ($x.data('datatype') === 'Double') { 
                v = parseFloat($(x).val());
            } else v = $x.val();
            res[$x.attr('name')] = v;
        }
    });
    return res;
}

function populateSelect ($sel, opts) {
    $.each(opts, function (i, v) {
        $sel.append($("<option/>").val(v).text(v));
    });
    $sel.selectpicker("refresh");
}

function resetStyles ($sel) {
    $sel.removeClass('label-warning').addClass('label-info');
}

function displayPrices ($cells, prices) {
    $cells.each(function(i,v) {
        var $v = $(v);
        if (prices[i] !== null) {
            $v.html(prices[i].toFixed(4));
        } else {
            $v.removeClass('label-info').addClass('label-warning');
            $v.html('Not available');
        }
    });
}

function processing (run) {
    var $spinner = $('.spinner');
    var $label = $('.processing-label');
    if (run) {
        $label.html('Processing...');
        $spinner.spin({width: 3, radius: 5, length: 5});
    } else {
        $spinner.spin(false);
        $label.empty();
    }
}

$(document).ready(function() {
    $('.selectpicker').selectpicker();
    $('.date').datepicker({autoclose: true,
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
    });
    $('#run').click(function() {
        resetStyles($('.price-output'));
        $('.price-output').empty();
        $('.total-output').empty();
        processing(true);
        var data = collectData($('.form-control'));
        var url = '/pricer/';
        $.post(url, { 'conf' : JSON.stringify(data) })
            .done(function(resp) { 
                displayPrices($('.price-output'), resp.prices);
                $('.total-output').html(resp.total.toFixed(4));
            })
            .fail(function(jqXHR, textStatus, errorThrown) {
                $('#error').html(jqXHR.responseText);
                $('#error').show();
            })
            .always(function() {processing(false)});
    });
    $.get('/marketData/underlyings/', function (data) {populateSelect($("select[data-datatype='Underlying']"), data)});
    $('.del-pfitem').click(function() {
        $.ajax({
            type: 'DELETE',
            url: '/portfolio/' + $(this).data('id')
        })
        .done(function() {
            location.reload();
        });
    });
    $('.del-item').click(function(evt) {
        evt.preventDefault();
        var data = [ 
             $(this).data('und'), 
             $(this).data('date')
        ];
        $.ajax({
            type: 'DELETE',
            url: $(this).attr('href'),
            data: JSON.stringify(data)
        })
         .done(function() {
             location.reload();
         })
         .fail(function(jqXHR, textStatus, errorThrown) {
             $('#error').html(jqXHR.responseText);
             $('#error').show();
         });
    });
    $('#add-data').click(function(evt) {
        evt.preventDefault();
        $('#result').empty().hide();
        $('#error').empty().hide();
        var url = $(this).attr('href');
        var data = collectData($('#add-data').closest('tr').find('.form-control'));
        $.post(url, JSON.stringify(data))
            .done(function(resp) { $('#result').html(resp.msg);
                                   $('#result').show();
                                   location.reload();
                                 })
            .fail(function(jqXHR, textStatus, errorThrown) {
                $('#error').html(jqXHR.responseText);
                $('#error').show();
            })
    })
});
