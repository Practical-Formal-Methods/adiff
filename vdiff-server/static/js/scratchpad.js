
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

$(document).ready(function() {
    $('select').formSelect();
});
