HTMLWidgets.widget({

    name: "handsontable4r",

    type: "output",

    factory: function (el, width, height) {



        return {
            renderValue: function (x) {

                // convert json to array
                if (x.data.length > 0 && x.data[0].constructor === Array) {
                    x.data = x.data;
                } else {
                    x.data = toArray(x.data.map(function (d) {
                        return x.colHeaders.map(function (ky) {
                            return d[ky];
                        });
                    }));
                }

                // console.log(x);
                var container = document.getElementById(el.id);
                var hot = new Handsontable(container, {
                    data: x.data,
                    rowHeaders: true,
                    colHeaders: x.colHeaders,
                    columns: x.columns,
                    filters: true,
                    dropdownMenu: true,
                    licenseKey: 'non-commercial-and-evaluation'
                });

            },

            resize: function (width, height) {


            }
        };
    }
});

function customRenderer(instance, TD, row, col, prop, value, cellProperties) {
    if (['date', 'handsontable', 'dropdown'].indexOf(cellProperties.type) > -1) {
        type = 'autocomplete';
    } else {
        type = cellProperties.type;
    }
    Handsontable.renderers.getRenderer(type)(instance, TD, row, col, prop, value, cellProperties);
}

// https://stackoverflow.com/questions/22477612/converting-array-of-objects-into-array-of-arrays
function toArray(input) {
    var result = input.map(function (obj) {
        return Object.keys(obj).map(function (key) {
            return obj[key];
        });
    });
    return result;
}