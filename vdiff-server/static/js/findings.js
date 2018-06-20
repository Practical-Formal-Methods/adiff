chipOptions = {
    placeholder: "verifiers",
    autocompleteOptions: {
        data: {
            'Klee': null,
            'Cbmc': null,
            'Smack': null
        },
        minLength: 0
    }
};

/* enable the select form */
document.addEventListener('DOMContentLoaded', function() {
    /* selects */
    var selectElems     = document.querySelectorAll('select');
    var selectInstances = M.FormSelect.init(selectElems);

    /* chips */
    var chipElems      = document.querySelectorAll('.chips');
    var chipsInstances  = M.Chips.init(chipElems, chipOptions);

    /* restore UI from query */
    var query = decodeURIComponent(window.location.search.substr(3));
    queryObj = adjustControls(query);
});


re = /Query (SuspicionIncomplete|SuspicionUnsound) (Majority|(AnyOf []))/

function adjustControls(s) {
    if (s == "everything") {
        console.log("adjust everything");
    } else {
        var m = s.match(re);
        console.log(m);
        // if (!m) {

        // } else {
        //     console.log("could not match");
        // }
    }
}
