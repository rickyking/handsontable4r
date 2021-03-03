HTMLWidgets.widget({

    name: "handsontable4r",

    type: "output",

    factory: function (el, width, height) {



        return {
            renderValue: function (x) {
                console.log(x)

                if (x.isHeatmap === true) {
                    x.afterLoadData = this.initHeatmap;
                    x.beforeChangeRender = this.updateHeatmap;
                }


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
                var hot = new Handsontable(container, x);

            },

            resize: function (width, height) {


            },
            // see http://handsontable.com/demo/heatmaps.html
            initHeatmap: function (firstTime, source) {
                this.heatmap = [];

                for (var i = 0, colCount = this.countCols(); i < colCount; i++) {
                    this.heatmap[i] = generateHeatmapData.call(this, i);
                }
            },

            updateHeatmap: function (change, source) {
                this.heatmap[change[0][1]] = generateHeatmapData.call(this, change[0][1]);
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

function generateHeatmapData(colId) {

    var values = this.getDataAtCol(colId);

    return {
        min: Math.min.apply(null, values),
        max: Math.max.apply(null, values)
    };
}