$(document).ready(function() {
    $('select').formSelect();
    $('#timeout').on('change', function() { $('#runAll').removeClass("disabled");});
});

function runVerifier(v) {

    var resultField = $("td#result-for-" + v);
    var button = $("button#button-for-" + v);

    var query = { source : $("#code").val(),
                  verifier: v,
                  timeout: $('#timeout').val()
                };

    $(button).addClass("disabled");

    $(resultField).find(".spinner").addClass("active");

    $(resultField).load("/run-verifier/", query, function() {
        console.log("got result for " + v);
        $(button).removeClass("disabled");
        $(resultField).find(".spinner").removeClass("active");
    });
}

function runAll() {
    $('.run-button').trigger('click');
    $('#runAll').addClass("disabled");
}

