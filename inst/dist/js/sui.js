'use strict';
// This needs to be cleaned up. Right now it's more a scratch pad than a JS library

// for the noUI multi-range inputs
function suiMultiRangeInputs() {
  const ranges = document.querySelectorAll('div.sui-multi-slider');
  for (let range of ranges) {
    noUiSlider.create(range, {
      start: [range.dataset.sliderMin, range.dataset.sliderMax],
      connect: true,
      tooltips: true,
      // direction: 'rtl',
      format: {
        to: value => Math.round(value),
        from: value => value
      },
      pips: {
        mode: 'positions',
        values: [0, 100],
        density: 25
      },
      range: { 'min': [Number(range.dataset.sliderMin)], 'max': [Number(range.dataset.sliderMax)] }
    });
  }
}

// for the noUI single range inputs
function suiRangeInputs() {
  const ranges = document.querySelectorAll('div.sui-slider');
  for (let range of ranges) {
    noUiSlider.create(range, {
      start: [range.dataset.sliderMin],
      connect: [true, false],
      tooltips: true,
      // direction: 'rtl',
      format: {
        to: value => Math.round(value),
        from: value => value
      },
      pips: {
        mode: 'positions',
        values: [0, 100],
        density: 25
      },
      range: { 'min': [Number(range.dataset.sliderMin)], 'max': [Number(range.dataset.sliderMax)] }
    });
  }
}

// for SUI multi-selection dropdown selection
function handleMultiSelection(dropdown, item, button, tagList) {
  handleSelection(dropdown, item, button);
  //TODO: needs some validation logic to disallow selecting the same item more than once
  let newTag = document.createElement('li');
  newTag.innerHTML =
    `<span class="uk-label sui-tag">${item.innerHTML}<span uk-icon="icon:close;ratio:0.5;"></span></span>`;
  newTag.onclick = (evt) => { newTag.remove(); };
  tagList.appendChild(newTag);
}

// for SUI single selection drodown selection
function handleSelection(dropdown, item, button) {
  button.innerHTML = item.innerHTML;
  UIkit.dropdown(dropdown).hide();
}

// for SUI Sidebar nav component
function suiSideNavBar() {
  const sidebarNav = document.querySelector('.sui-nav-sidebar');
  for (let item of sidebarNav.children) {
    if (item.children[0].nodeName === "A") {
      item.children[0].addEventListener('click', (evt) => {
        evt.preventDefault();
        for (let bar of sidebarNav.children) {
          bar.classList.remove('uk-active');
        }
        item.classList.add('uk-active');
      });
    }
  }
  sidebarNav.children[1].classList.add('uk-active');
}

// for SUI segmented button control component
function suiSegmentedButtons() {
  const segmentedControls = document.querySelectorAll('.sui-segmented-button');
  for (let segControl of segmentedControls) {

    //initialize the first option as the selected option
    segControl.children[0].classList.add('sui-segmented-button-selected');

    // assign a click event listener to control the selected state
    for (let segment of segControl.children) {
      segment.addEventListener('click', () => {
        let selected = segControl.querySelector('.sui-segmented-button-selected');
        if (selected !== segment) {
          selected.classList.remove('sui-segmented-button-selected');
          segment.classList.add('sui-segmented-button-selected');
        }
      });
    }
  }
}