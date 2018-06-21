
$(document).ready(function() {
    $('select').formSelect();
    /* restore UI from query */
    var query = decodeURIComponent(window.location.search.substr(3));
    queryObj = adjustControls(query);

    /* bind search button */
    $('#searchButton').on('click', reloadQuery);
    $('#accordingTo').on('change', accordingToChanged);

});

re_query = /Query (SuspicionIncomplete|SuspicionUnsound) (Majority|\(AnyOf (.*)\)|\(AllOf (.*)\))/;


function adjustControls(s) {
    if (s == "everything") {
        console.log("adjust everything");
    } else {
        var m = s.match(re_query);
        console.log(m);
        if (m) {
            console.log(m[1]);
            var suspicion = m[1];
            setSuspicion(suspicion);
            var mode = m[2];

            if (mode.startsWith("(AnyOf") || mode.startsWith("(AllOf")) {
                var vl = m[3];
                var verifiers = vl.substr(1, vl.length -2).split(",").map(x => x.substr(1, x.length-2));
                setVerifiers(verifiers);
            } else if (mode == "Majority") {
                setMode("Majority");
            }

        } else {
            console.log("could not parse query");
        }
    }
}

function setSuspicion(s) {
    console.log("setting suspicion to " + s);
    $('#suspicion > option').prop('selected', false);
    $('#suspicion option[value="' + s + '"]').prop('selected', true);
    /* re-init */
    $('#suspicion').formSelect();
}

function setMode(s) {
    console.log("setting mode to " + s);
    if (s == 'Majority') {
        $('#accordingTo option').prop('selected', false);
        $('#accordingTo option[value="' + s + '"]').prop('selected', true);
        $('#accordingTo').formSelect();
        $('#verifier-row').hide();

    }
}

function setVerifiers(vs) {
    console.log("setVerifiers");
    console.log(vs);

    $('#verifiers option').each(function(i, elem){
        var v = $(elem).val();
        if ($.inArray(v, vs) != -1) {
            $(elem).prop('selected', true);
        } else {
            $(elem).prop('selected', false);
        }
    });
    $('#verifiers').formSelect();
}

function reloadQuery() {
    var suspicion = $('#suspicion').val();
    var accordingTo = $('#accordingTo').val();
    var q = "";

    if (accordingTo == 'Majority') {
        q = "Query " + suspicion + " Majority";
    } else {
        /* get instance */
        var verifiers = $('#verifiers').val();
        console.log(verifiers);
        var verifiersL = verifiers.map(function(v) {
            return '"' + v + '"';
        }).join(',');
        console.log(verifiersL);
        q = "Query " + suspicion + " (" + accordingTo + " [" + verifiersL + "])";
    }

    window.location = "?q=" + encodeURIComponent(q);
}

function accordingToChanged() {
    var x = $('#accordingTo').val();
    if (x == 'Majority') {
        $('#verifier-row').hide();
    } else {
        $('#verifier-row').show();
    }
}
