HTMLWidgets.widget({

    name: "handsontable4r",

    type: "output",

    factory: function (el, width, height) {

        var container = document.getElementById(el.id);
        var hot = new Handsontable(container, {
            licenseKey: "non-commercial-and-evaluation"
        });

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
                var sort_settings;
                var filter_collection;
                x.beforeLoadData = function (change, source) {
                    console.log("beforeLoadData")
                    var plugin = this.getPlugin('multiColumnSorting');
                    if (plugin.isSorted()) {
                        sort_settings = plugin.getSortConfig();
                    };
                    var filtersPlugin = hot.getPlugin('filters');
                    obj = filtersPlugin.conditionCollection;
                    if (!jQuery.isEmptyObject(obj)) {
                        filter_collection = JSON.parse(JSON.stringify(obj.filteringStates.indexedValues));
                    };

                };
                x.afterChange = function (change, source) {
                    console.log("after change")
                    var plugin = this.getPlugin('multiColumnSorting');
                    if (sort_settings) {
                        plugin.sort(sort_settings);
                    };
                    var filtersPlugin = hot.getPlugin('filters');
                    if (!jQuery.isEmptyObject(filter_collection)) {
                        // filtersPlugin.conditionCollection = Object.assign({}, filter_collection);
                        conditions = Object.assign({}, filter_collection);

                        for (let [key, value] of Object.entries(conditions)) {
                            if (value) {
                                column_id = key;
                                operation_id = value.operation;
                                value.conditions.forEach(function (value, index) {
                                    filtersPlugin.addCondition(column_id, value.name, value.args, operation_id)
                                });
                            }
                        }

                        filtersPlugin.filter();
                    };

                };
                // console.log(x);
                hot.params = x;
                hot.updateSettings(x);

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
            },

            hot: hot
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