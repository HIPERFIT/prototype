function collectData (ins) {
    var res = {};
    var v;
    $.each(ins, function (i, x) {
        var $x = $(x);
        if ($x.val() !== "") {
            var datatype = $x.data('datatype');
            if ((datatype === 'Double') || (datatype === 'PercentField')) { 
                v = parseFloat($x.val());
            } else if (datatype === 'Bool') {
                v = $x.prop('checked');
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

function resetStyles () {
    $('.price-output').removeClass('label-warning').addClass('label-info');
    $('.total-output').removeClass('label-warning').addClass('label-success');
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
        $('.price-output').removeClass('label-info').addClass('label-warning');
        $('.total-output').removeClass('label-success').addClass('label-warning');
        $spinner.spin({width: 3, radius: 5, length: 5});
    } else {
        $spinner.spin(false);
        $label.empty();
    }
}

function invalidateResult ($sel) {
    // trying to determine whether valuation has been done at least once
    if (!$('.total-output').is(':empty')) {
        $sel.removeClass('label-info').addClass('label-warning');
        $('#pricing-form-alert').html('Parameters have been changed. Please run valuation.');
        $('#pricing-form-alert').show();
    }
}

function postData ($inputs, url) {
    $('#result').empty().hide();
    $('#error').empty().hide();
    var data = collectData($inputs);
    $.post(url, JSON.stringify(data))
        .done(function(resp) { 
            location.reload();
        })
        .fail(function(jqXHR, textStatus, errorThrown) {
            $('#error').html(jqXHR.responseText);
            $('#error').show();
        })
}

function hideAlerts() {
    $('#pricing-form-alert, #error, #result').empty().hide();
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
        var contract = collectData($('.contract-data .form-control, input[type="checkbox"]'));
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
        resetStyles();
        hideAlerts();
        processing(true);
        var data = collectData($('.form-control'));
        var url = '/pricer/';
        $.post(url, { 'conf' : JSON.stringify(data) })
            .done(function(resp) {
                resetStyles();
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
        var data = $(this).data('key');
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
        postData($('#add-data').closest('tr').find('.form-control'), $(this).attr('href'));
    });
    $('#add-data-corrs').click(function(evt) {
        evt.preventDefault();
        postData($('#add-data-corrs').closest('tr').find('.form-control'), 
                 $(this).attr('href'))
    });

    var url = document.location.toString();
    if (url.match('marketData') && url.match('#')) {
        $('#data-tabs a[href=#'+url.split('#')[1]+']').tab('show') ;
    }
    $('#data-tabs a.tab-link').on('shown.bs.tab', function (e) {
        window.location.hash = e.target.hash;
    })
    $('input[name="currentDate"], input[name="interestRate"], input[name="iterations"]').keydown(function() {invalidateResult($('.price-output, .total-output'))});
    $('input[name="currentDate"]').change(function() {invalidateResult($('.price-output, .total-output'))});
})
