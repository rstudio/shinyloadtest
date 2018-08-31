'use strict';

// DO NOT EDIT BY HAND. EDIT shinyloadtest-es6.js and run through https://babeljs.io/en/repl.
// Copy results below

(function () {

  // Run through https://babeljs.io/en/repl.
  // Copy results to ./js/shinyloadtest.js


  suiSegmentedButtons();
  suiSideNavBar();
  managedChartGrid(document.querySelector('#slowest-min-picker'), document.querySelector('#slowest-min-grid'));
  managedChartGrid(document.querySelector('#slowest-max-picker'), document.querySelector('#slowest-max-grid'));
  managedChartGrid(document.querySelector('#largest-mean-diff-picker'), document.querySelector('#largest-mean-diff-grid'));

  managedChartGrid(document.querySelector('#concurrency-slope-picker'), document.querySelector('#concurrency-slope-grid'));
  managedChartGrid(document.querySelector('#concurrency-intercept-picker'), document.querySelector('#concurrency-intercept-grid'));
  managedChartGrid(document.querySelector('#concurrency-error-picker'), document.querySelector('#concurrency-error-grid'));

  // app specific handler for changing the number of box plots to show
  // this is so ugly but it's quick and this is just a prototype, but still :**(
  var boxPlotTemplate = '<div class=""><div class="uk-card uk-card-small slt-chart-grid-chart">\n<img src="SVG/boxplot.svg" style="width:100%;"/></div></div>';

  // this is the script that needs to run in the actual app to manage the generated SVGs
  function managedChartGrid(picker, activeGrid) {
    var charts = [];
    if (!picker && !activeGrid) {
      return;
    }
    var _iteratorNormalCompletion = true;
    var _didIteratorError = false;
    var _iteratorError = undefined;

    try {
      for (var _iterator = activeGrid.children[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
        var node = _step.value;

        charts.push(node);
      }
    } catch (err) {
      _didIteratorError = true;
      _iteratorError = err;
    } finally {
      try {
        if (!_iteratorNormalCompletion && _iterator.return) {
          _iterator.return();
        }
      } finally {
        if (_didIteratorError) {
          throw _iteratorError;
        }
      }
    }

    var updateChartGrid = function updateChartGrid() {
      // Check to make sure we aren't outside the bounds of the chart set
      var currentValue = Number(picker.value);
      var minValue = Number(picker.min),
          maxValue = Number(picker.max);

      if (currentValue > maxValue) currentValue = maxValue;else if (currentValue < minValue) currentValue = minValue;

      var visibleCharts = activeGrid.querySelectorAll('.slt-visible-chart').length;
      if (currentValue > visibleCharts) {
        for (var i = visibleCharts; i < currentValue; i++) {
          charts[i].classList.add('slt-visible-chart');
        }
      } else if (currentValue < visibleCharts) {
        for (var _i = visibleCharts - 1; _i >= currentValue; _i--) {
          charts[_i].classList.remove('slt-visible-chart');
        }
      }
    };
    picker.addEventListener('input', updateChartGrid);
    // this sets the initial state of the DOM
    updateChartGrid();
  }

  // core logic to change views with a segmented button control
  var switchSegmentViews = function switchSegmentViews(viewsContainer, selectedView) {
    var _iteratorNormalCompletion2 = true;
    var _didIteratorError2 = false;
    var _iteratorError2 = undefined;

    try {
      for (var _iterator2 = viewsContainer[Symbol.iterator](), _step2; !(_iteratorNormalCompletion2 = (_step2 = _iterator2.next()).done); _iteratorNormalCompletion2 = true) {
        var view = _step2.value;

        view.style.display = 'none';
      }
    } catch (err) {
      _didIteratorError2 = true;
      _iteratorError2 = err;
    } finally {
      try {
        if (!_iteratorNormalCompletion2 && _iterator2.return) {
          _iterator2.return();
        }
      } finally {
        if (_didIteratorError2) {
          throw _iteratorError2;
        }
      }
    }

    document.querySelector('#' + selectedView).style.display = 'block';
  };

  // app specific click handler for #sessions segmented control
  var add_tab_click_handler = function add_tab_click_handler(name) {
    var controllerSegments = document.querySelector('#' + name + '-report-controller').children;
    var charts = document.querySelector('#' + name + '-chart-view').children;
    var _iteratorNormalCompletion3 = true;
    var _didIteratorError3 = false;
    var _iteratorError3 = undefined;

    try {
      var _loop = function _loop() {
        var segment = _step3.value;

        segment.addEventListener('click', function () {
          switchSegmentViews(charts, segment.dataset.viewOption);
        });
      };

      for (var _iterator3 = controllerSegments[Symbol.iterator](), _step3; !(_iteratorNormalCompletion3 = (_step3 = _iterator3.next()).done); _iteratorNormalCompletion3 = true) {
        _loop();
      }
    } catch (err) {
      _didIteratorError3 = true;
      _iteratorError3 = err;
    } finally {
      try {
        if (!_iteratorNormalCompletion3 && _iterator3.return) {
          _iterator3.return();
        }
      } finally {
        if (_didIteratorError3) {
          throw _iteratorError3;
        }
      }
    }

    switchSegmentViews(charts, controllerSegments[0].dataset.viewOption);
  };
  add_tab_click_handler("sessions");
  add_tab_click_handler("session-duration");
  add_tab_click_handler("waterfall");
  add_tab_click_handler("latency");
  add_tab_click_handler("duration");
  add_tab_click_handler("concurrency");

  // app specific handler to side menu
  var testSelector = document.querySelector('.sui-nav-sidebar');
  var _iteratorNormalCompletion4 = true;
  var _didIteratorError4 = false;
  var _iteratorError4 = undefined;

  try {
    var _loop2 = function _loop2() {
      var item = _step4.value;

      var link = item.children[0];
      if (link.href) {
        link.addEventListener('click', function () {
          var newActivePage = document.querySelector('#' + link.href.split('#')[1]);
          var _iteratorNormalCompletion5 = true;
          var _didIteratorError5 = false;
          var _iteratorError5 = undefined;

          try {
            for (var _iterator5 = document.querySelectorAll('section')[Symbol.iterator](), _step5; !(_iteratorNormalCompletion5 = (_step5 = _iterator5.next()).done); _iteratorNormalCompletion5 = true) {
              var section = _step5.value;

              section.style.display = 'none';
            }
          } catch (err) {
            _didIteratorError5 = true;
            _iteratorError5 = err;
          } finally {
            try {
              if (!_iteratorNormalCompletion5 && _iterator5.return) {
                _iterator5.return();
              }
            } finally {
              if (_didIteratorError5) {
                throw _iteratorError5;
              }
            }
          }

          newActivePage.style.display = 'block';
        });
      }
    };

    for (var _iterator4 = testSelector.children[Symbol.iterator](), _step4; !(_iteratorNormalCompletion4 = (_step4 = _iterator4.next()).done); _iteratorNormalCompletion4 = true) {
      _loop2();
    }
  } catch (err) {
    _didIteratorError4 = true;
    _iteratorError4 = err;
  } finally {
    try {
      if (!_iteratorNormalCompletion4 && _iterator4.return) {
        _iterator4.return();
      }
    } finally {
      if (_didIteratorError4) {
        throw _iteratorError4;
      }
    }
  }

  testSelector.children[1].children[0].click();

  // https://datatables.net/examples/plug-ins/sorting_auto.html
  $.fn.dataTable.ext.type.detect.unshift(function (d) {
    if (/Event \d+\)/.test(d)) {
      return "shinyloadtest-label";
    }
    return null;
  });
  $.fn.dataTable.ext.type.order['shinyloadtest-label-pre'] = function (d) {
    return Number(d.match(/^Event (\d+)\)/)[1]);
  };
  $("#event-duration-data-table > table").DataTable({
    paging: false,
    order: [[1, "desc"]]
  });
  $("#event-concurrency-data-table > table").DataTable({
    paging: false,
    order: [[1, "desc"]]
  });
})();
