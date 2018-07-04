$(document).ready(function() {
    $('select').formSelect();
    $('#timeout').on('change', function() { $('#runAll').removeClass("disabled");});

    var options = {
        maxLines:10000,
        wrap: true,
        mode: "ace/mode/c_cpp",
        theme: "ace/theme/Chrome"
    };
    editor = ace.edit("editor", options);
    editor.setValue($('#code').val());
});

function runVerifier(v) {

    var resultField = $("td#result-for-" + v);
    var button = $("button#button-for-" + v);

    var query = { source : editor.getSession().getValue(),
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

