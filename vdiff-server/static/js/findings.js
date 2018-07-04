
$(document).ready(function() {
    $('select').formSelect();
    /* restore UI from query */
    var urlParams = window.location.search;
    urlParams.substr(1).split("&").forEach(function (param) {
        ps = param.split("=");
        if (ps[0] == "q") {
            var query = decodeURIComponent(ps[1]);
            queryObj = adjustControls(query);
        } else if (ps[0] == "qf") {
            var qf = decodeURIComponent(ps[1]);
            console.log(qf);
            var vs = qf.substr(1, qf.length - 2).split(",").map(v => v.substr(1, v.length-2));
            console.log(vs);
            setFocusedVerifiers(vs);
        } else if (ps[0] == "page") {
            page = ps[1];
        }
    });

    /* bind search button */
    $('#searchButton').on('click', reloadQuery);
    $('#suspicion').on('change', suspicionChanged);
    $('#accordingTo').on('change', accordingToChanged);

});

re_query = /Query (SuspicionIncomplete|SuspicionUnsound) (Majority|\(AnyOf (.*)\)|\(AllOf (.*)\))/;


function adjustControls(s) {
    if (s == "Everything") {
        setSuspicion("Everything");
    } else if (s == "Disagreement") {
        setSuspicion("Disagreement");
    } else {
        var m = s.match(re_query);
        console.log(m);
        if (m) {
            console.log(m[1]);
            var suspicion = m[1];
            setSuspicion(suspicion);
            var mode = m[2];

            if (mode.startsWith("(AnyOf") || mode.startsWith("(AllOf")) {
                mode = mode.substr(1, mode.indexOf('[') - 2);
                setMode(mode);
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
    suspicionChanged();
}

function setMode(s) {
    console.log("setting mode to " + s);
    $('#accordingTo option').each(function(i,elem) {
        if ($(elem).val() == s) {
            console.log("x");
            $(elem).prop('selected', true);
        } else {
            console.log("y");
            $(elem).prop('selected', false);
        }
    });
    $('#accordingTo').formSelect();

    if (s == 'Majority') {
        $('#verifier-row').hide();
    } else {
        $('#verifier-row').show();
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

function setFocusedVerifiers(vs) {
    console.log("setFocusedVerifiers");
    console.log(vs);

    $('#focusVerifiers option').each(function(i, elem){
        var v = $(elem).val();
        if ($.inArray(v, vs) != -1) {
            $(elem).prop('selected', true);
        } else {
            $(elem).prop('selected', false);
        }
    });
    $('#focusVerifiers').formSelect();
}

function reloadQuery() {
    var suspicion = $('#suspicion').val();
    var accordingTo = $('#accordingTo').val();
    var q = "";

    /* build the 'q' string */
    if (suspicion == 'Everything' || suspicion == 'Disagreement') {
        q = suspicion;
    } else {
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
    }

    /* build the 'qf' string */
    var qf = "[";
    qf += $('#focusVerifiers').val().map(v => '"' + v +'"').join(",");
    qf += "]";

    window.location = "?q=" + encodeURIComponent(q) + "&qf=" + encodeURIComponent(qf);
}

function suspicionChanged() {
    var x = $('#suspicion').val();
    if (x == 'Everything' || x == 'Disagreement') {
        $('#accordingToBox').hide();
        $('#verifier-row').hide();
    } else {
        $('#accordingToBox').show();
        $('#accordingTo').trigger("change");
    }
}

function accordingToChanged() {
    var x = $('#accordingTo').val();
    if (x == 'Majority') {
        $('#verifier-row').hide();
    } else {
        $('#verifier-row').show();
    }
}
function gotoPage(n) {
    console.log("goto page " + n);
}
