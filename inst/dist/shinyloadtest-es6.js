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

// random int generator helper
const randInt = (min, max) => {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min +1)) + min;
};

{
  // generate a data table because no one wants to write a realistic length one by hand
  const tableBody = document.querySelector('#event-duration-data-table table tbody');
  for (let i = 0; i < 50; i++){
    let label = 'REQ_HOME';
    if( i > 0) {
      switch(randInt(1,4)){
        case 1:
          label = 'WS_RECV';
          break;
        case 2:
          label = 'WS_OPEN';
          break;
        case 3:
          label = 'REQ_GET';
          break;
        case 4:
          label = 'WS_RECV_INIT'
          break;
      }
    }
    let minTime = Math.random().toFixed(3);
    let maxTime = Math.random().toFixed(3) + randInt(0,20);
    let meanDiff =  Math.random().toFixed(3) + randInt(0, 7);
    const tableRowTemplate = `<tr><td>${i+1}:${label}</td><td>${minTime}</td>
<td>${maxTime}</td><td>${meanDiff}</td></tr>`;
    tableBody.insertAdjacentHTML('beforeend', tableRowTemplate);
  }
}
{
  // generate a data table because no one wants to write a realistic length one by hand
  const tableBody = document.querySelector('#event-concurrency-data-table table tbody');
  for (let i = 0; i < 50; i++){
    let label = 'REQ_HOME';
    if( i > 0) {
      switch(randInt(1,4)){
        case 1:
          label = 'WS_RECV';
          break;
        case 2:
          label = 'WS_OPEN';
          break;
        case 3:
          label = 'REQ_GET';
          break;
        case 4:
          label = 'WS_RECV_INIT'
          break;
      }
    }
    let minTime = Math.random().toFixed(3);
    let maxTime = Math.random().toFixed(3) + randInt(0,20);
    let meanDiff =  Math.random().toFixed(3) + randInt(0, 7);
    const tableRowTemplate = `<tr><td>${i+1}:${label}</td><td>${minTime}</td>
<td>${maxTime}</td><td>${meanDiff}</td></tr>`;
    tableBody.insertAdjacentHTML('beforeend', tableRowTemplate);
  }
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

})()
