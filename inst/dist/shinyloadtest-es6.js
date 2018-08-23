(function(){

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
const boxPlotTemplate = `<div class=""><div class="uk-card uk-card-small slt-chart-grid-chart">
<img src="SVG/boxplot.svg" style="width:100%;"/></div></div>`;

// this is the script that needs to run in the actual app to manage the generated SVGs
function managedChartGrid(picker, activeGrid) {
  let charts = [];
  if (!picker && !activeGrid) {
    return;
  }
  for( let node of activeGrid.children){
    charts.push(node);
  }
  const updateChartGrid = () => {
    // Check to make sure we aren't outside the bounds of the chart set
    let currentValue = Number(picker.value);
    const minValue = Number(picker.min),
      maxValue = Number(picker.max);

    if (currentValue > maxValue) currentValue = maxValue;
    else if (currentValue < minValue) currentValue = minValue;

    const visibleCharts = activeGrid.querySelectorAll('.slt-visible-chart').length;
    if (currentValue > visibleCharts) {
      for (let i = visibleCharts; i < currentValue; i++) {
        charts[i].classList.add('slt-visible-chart');
      }
    }
    else if (currentValue < visibleCharts) {
      for (let i = visibleCharts - 1; i >= currentValue; i--) {
        charts[i].classList.remove('slt-visible-chart');
      }
    }
  };
  picker.addEventListener('input', updateChartGrid);
  // this sets the initial state of the DOM
  updateChartGrid();
}


// core logic to change views with a segmented button control
const switchSegmentViews = (viewsContainer, selectedView) => {
  for(let view of viewsContainer){
    view.style.display = 'none';
  }
  document.querySelector(`#${selectedView}`).style.display = 'block';
};

// app specific click handler for #sessions segmented control
const add_tab_click_handler = (name) => {
  const controllerSegments = document.querySelector(`#${name}-report-controller`).children;
  const charts = document.querySelector(`#${name}-chart-view`).children;
  for (let segment of controllerSegments) {
    segment.addEventListener('click', () => {
      switchSegmentViews(charts, segment.dataset.viewOption);
    });
  }
  switchSegmentViews(charts, controllerSegments[0].dataset.viewOption);
}
add_tab_click_handler("sessions");
add_tab_click_handler("session-duration");
add_tab_click_handler("waterfall");
add_tab_click_handler("latency");
add_tab_click_handler("duration");
add_tab_click_handler("concurrency");

// app specific handler to side menu
const testSelector = document.querySelector('.sui-nav-sidebar');
for(let item of testSelector.children){
  let link = item.children[0];
  if(link.href){
    link.addEventListener('click', () => {
      let newActivePage = document.querySelector(`#${link.href.split('#')[1]}`);
      for(let section of document.querySelectorAll('section')){
        section.style.display = 'none';
      }
      newActivePage.style.display = 'block';

    });
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

})()
