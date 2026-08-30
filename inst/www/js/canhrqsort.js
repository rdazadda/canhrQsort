/**
 * canhrQsort Dashboard - JavaScript Utilities
 * canhrActi design system: sidebar collapse, toasts, tab sync.
 */

(function() {
  'use strict';

  // Sidebar collapse / mobile drawer (ported from canhrActi)

  var SIDEBAR_KEY = 'canhrQsort.sidebar.collapsed';
  var BREAKPOINT = 992;

  function isWide() { return window.innerWidth >= BREAKPOINT; }

  function initSidebarToggle() {
    var body = document.body;
    var toggleBtn = document.querySelector('.sidebar-toggle');
    if (!toggleBtn || toggleBtn.dataset.canhrBound === '1') return;
    toggleBtn.dataset.canhrBound = '1';

    function setCollapsed(collapsed, persist) {
      body.classList.toggle('sidebar-collapse', collapsed);
      if (persist) localStorage.setItem(SIDEBAR_KEY, String(collapsed));
      toggleBtn.setAttribute('aria-expanded', String(!collapsed));
    }

    function setOpen(open) {
      body.classList.toggle('sidebar-open', open);
      toggleBtn.setAttribute('aria-expanded', String(open));
    }

    if (isWide()) {
      setCollapsed(localStorage.getItem(SIDEBAR_KEY) === 'true', false);
    }

    toggleBtn.addEventListener('click', function(e) {
      e.preventDefault();
      if (isWide()) {
        setCollapsed(!body.classList.contains('sidebar-collapse'), true);
      } else {
        setOpen(!body.classList.contains('sidebar-open'));
      }
    });

    document.addEventListener('click', function(e) {
      if (!isWide() && body.classList.contains('sidebar-open')) {
        if (!e.target.closest('.main-sidebar') && !e.target.closest('.sidebar-toggle')) {
          setOpen(false);
        }
      }
    });

    document.addEventListener('keydown', function(e) {
      if (e.key === 'Escape' && body.classList.contains('sidebar-open')) {
        setOpen(false);
      }
    });

    window.addEventListener('resize', function() {
      if (isWide() && body.classList.contains('sidebar-open')) {
        body.classList.remove('sidebar-open');
      }
    });
  }

  // Toast Notifications

  function showToast(message, type, duration) {
    var container = document.getElementById('toast-container') || createToastContainer();

    var toast = document.createElement('div');
    toast.className = 'toast toast-' + type;
    toast.innerHTML =
      '<div class="toast-icon">' +
        '<i class="fa fa-' + getToastIcon(type) + '"></i>' +
      '</div>' +
      '<div class="toast-message">' + message + '</div>' +
      '<button class="toast-close" onclick="this.parentElement.remove()">' +
        '<i class="fa fa-times"></i>' +
      '</button>';

    container.appendChild(toast);

    setTimeout(function() { toast.classList.add('show'); }, 10);

    setTimeout(function() {
      toast.classList.remove('show');
      setTimeout(function() { toast.remove(); }, 300);
    }, duration);
  }

  function createToastContainer() {
    var container = document.createElement('div');
    container.id = 'toast-container';
    container.className = 'toast-container';
    document.body.appendChild(container);
    return container;
  }

  function getToastIcon(type) {
    var icons = {
      success: 'check-circle',
      error: 'exclamation-circle',
      warning: 'exclamation-triangle',
      info: 'info-circle'
    };
    return icons[type] || icons.info;
  }

  // Shiny Custom Handlers

  function initShinyHandlers() {
    if (typeof Shiny === 'undefined') return;

    Shiny.addCustomMessageHandler('showToast', function(data) {
      showToast(data.message, data.type || 'info', data.duration || 3000);
    });

    // Header status badge -> canhrActi file-badge
    Shiny.addCustomMessageHandler('updateHeaderStatus', function(status) {
      var badge = document.getElementById('header_status');
      if (!badge) return;
      badge.innerHTML = '<i class="fa fa-' + status.icon + '"></i> ' + status.text;
      badge.classList.remove('file-badge-empty', 'file-badge-active');
      badge.classList.add(status.active ? 'file-badge-active' : 'file-badge-empty');
    });

    // Toggle a CSS class on an element
    Shiny.addCustomMessageHandler('toggleClass', function(data) {
      var el = document.getElementById(data.id);
      if (el) el.classList.toggle(data.className);
    });

    // Switch data preview tabs (table vs pyramid) - details-tab styling
    Shiny.addCustomMessageHandler('switchDataTab', function(data) {
      var tabTable = document.getElementById(data.tabTableId);
      var tabPyramid = document.getElementById(data.tabPyramidId);
      var panelTable = document.getElementById(data.panelTableId);
      var panelPyramid = document.getElementById(data.panelPyramidId);

      if (data.activeTab === 'table') {
        if (tabTable) tabTable.classList.add('active');
        if (tabPyramid) tabPyramid.classList.remove('active');
        if (panelTable) panelTable.style.display = '';
        if (panelPyramid) panelPyramid.style.display = 'none';
      } else {
        if (tabTable) tabTable.classList.remove('active');
        if (tabPyramid) tabPyramid.classList.add('active');
        if (panelTable) panelTable.style.display = 'none';
        if (panelPyramid) panelPyramid.style.display = '';
        setTimeout(function() { $(window).trigger('resize'); }, 100);
      }
    });

    // Drop-zone forwarding: Shiny parks the real file input off-screen, so
    // files dropped on the zone are handed to it directly.
    document.addEventListener('dragover', function(e) {
      var zone = e.target.closest ? e.target.closest('.ov2-dropzone') : null;
      if (zone) {
        e.preventDefault();
        zone.classList.add('drag-over');
      }
    });
    document.addEventListener('dragleave', function(e) {
      var zone = e.target.closest ? e.target.closest('.ov2-dropzone') : null;
      if (zone && !zone.contains(e.relatedTarget)) {
        zone.classList.remove('drag-over');
      }
    });
    document.addEventListener('drop', function(e) {
      var zone = e.target.closest ? e.target.closest('.ov2-dropzone') : null;
      if (!zone) return;
      e.preventDefault();
      zone.classList.remove('drag-over');
      var input = zone.querySelector('input[type="file"]');
      if (input && e.dataTransfer && e.dataTransfer.files && e.dataTransfer.files.length) {
        input.files = e.dataTransfer.files;
        input.dispatchEvent(new Event('change', { bubbles: true }));
      }
    });

    // Generic underline-tab switcher for panels built from ov2-tab links
    // Client-side tab switching: works instantly even while R is busy
    window.canhrPrTab = function(el, tabs, panels) {
      var active = tabs.indexOf(el.id);
      if (active < 0) return false;
      tabs.forEach(function(id, i) {
        var t = document.getElementById(id);
        if (t) t.classList.toggle('active', i === active);
      });
      panels.forEach(function(id, i) {
        var p = document.getElementById(id);
        if (p) p.style.display = (i === active) ? '' : 'none';
      });
      // Wake suspended outputs in the revealed panel: Shiny re-checks
      // visibility on resize, and different widgets listen on the jQuery
      // event or the native one, so fire both
      setTimeout(function() {
        $(window).trigger('resize');
        window.dispatchEvent(new Event('resize'));
      }, 100);
      return false;
    };

    // Set or clear one class on one element (mode switching)
    Shiny.addCustomMessageHandler('setClassIf', function(d) {
      var el = document.getElementById(d.id);
      if (el) el.classList.toggle(d.className, !!d.on);
    });

    // Factors stepper: mark step states and show the current panel.
    // Server-driven so locking follows analysis state; DOM is never rebuilt.
    window.canhrFrStep = function(steps, panels, cur, unlocked) {
      steps.forEach(function(id, idx) {
        var el = document.getElementById(id);
        if (!el) return;
        var i = idx + 1;
        el.classList.toggle('done', i < cur);
        el.classList.toggle('cur', i === cur);
        el.classList.toggle('locked', i > unlocked);
      });
      panels.forEach(function(id, idx) {
        var el = document.getElementById(id);
        if (!el) return;
        el.style.display = (idx + 1 === cur) ? '' : 'none';
      });
      setTimeout(function() { $(window).trigger('resize'); }, 100);
    };

    // Details disclosure: instant client-side toggle; the resize nudge lets
    // plotly widgets hidden at render time size themselves on first open
    window.canhrFrToggle = function(id) {
      var el = document.getElementById(id);
      if (el) {
        el.classList.toggle('fr-open');
        setTimeout(function() { $(window).trigger('resize'); }, 100);
      }
      return false;
    };

    // Sidebar active item sync (li.active like shinydashboard)
    Shiny.addCustomMessageHandler('syncSidebar', function(tabValue) {
      var tabToLinkId = {
        'home': 'nav_home',
        'analyze': 'nav_analyze',
        'bayesian': 'nav_bayesian',
        'visualize': 'nav_visualize'
      };

      document.querySelectorAll('.sidebar-menu > li').forEach(function(li) {
        li.classList.remove('active');
      });

      var linkId = tabToLinkId[tabValue];
      if (linkId) {
        var activeLink = document.getElementById(linkId);
        if (activeLink) {
          var li = activeLink.closest('li');
          if (li) li.classList.add('active');
        }
      }

      // Update the header page title
      var titles = {
        'home': 'Overview',
        'analyze': 'Frequentist',
        'bayesian': 'Bayesian',
        'visualize': 'Visualization'
      };
      var titleEl = document.getElementById('header_page_title');
      if (titleEl && titles[tabValue]) {
        titleEl.textContent = titles[tabValue];
      }
    });
  }

  // Initialize Everything

  function init() {
    document.documentElement.setAttribute('data-bs-theme', 'light');
    // canhrActi's stylesheet scopes several shell rules to shinydashboard's
    // skin class; adding it makes the whole cascade resolve identically.
    document.body.classList.add('skin-blue');
    initSidebarToggle();
    initShinyHandlers();
    console.log('canhrQsort dashboard initialized (canhrActi design system)');
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }

  if (typeof Shiny !== 'undefined') {
    $(document).on('shiny:connected', function() {
      setTimeout(initSidebarToggle, 100);
    });
  }

  window.canhrQsort = {
    showToast: showToast
  };

})();
