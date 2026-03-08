
dashboardCSS <- "
/* ============================================
   DASHBOARD STYLING
   ============================================ */

:root {
  --primary: #2E86AB;
    --primary-dark: #1A5F7A;
    --secondary: #A23B72;
    --accent: #F18F01;
    --success: #06A77D;
    --danger: #D62828;
    --sidebar-bg: #1E2A38;
    --content-bg: #F4F6F9;
}

/* Dashboard Body */
  .content-wrapper {
    background-color: var(--content-bg) !important;
  }

/* Main Sidebar */
  .skin-blue .main-sidebar {
    background-color: var(--sidebar-bg) !important;
  }

.skin-blue .sidebar-menu > li.active > a {
  border-left-color: var(--accent) !important;
  background: linear-gradient(90deg, rgba(241,143,1,0.15) 0%, transparent 100%) !important;
}

.skin-blue .sidebar-menu > li:hover > a {
  background: rgba(255, 255, 255, 0.05) !important;
  border-left-color: var(--accent) !important;
}


/* Improve sidebar + navbar text consistency */
  .sidebar-menu > li > a,
.main-header .navbar,
.main-header .logo {
  font-family: 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
}


/* Gradient header override */
  .skin-purple .main-header .navbar {
    background: linear-gradient(135deg, var(--primary) 0%, var(--secondary) 100%) !important;
  }
.skin-purple .main-header .logo {
  background: linear-gradient(135deg, var(--primary) 0%, var(--secondary) 100%) !important;
  color: white !important;
}
.skin-purple .main-header .logo:hover {
  background: linear-gradient(135deg, var(--primary) 0%, var(--secondary) 100%) !important;
}


/* Info Boxes */
  .info-box {
    border-radius: 10px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.12);
    transition: all 0.3s ease;
    border-top: 3px solid;
  }

.info-box:hover {
  transform: translateY(-5px);
  box-shadow: 0 8px 20px rgba(0,0,0,0.18);
}

.info-box-icon {
  border-radius: 10px 0 0 10px;
}

/* Value Boxes */
  .small-box {
    border-radius: 10px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.12);
    transition: all 0.3s ease;
  }

.small-box:hover {
  transform: translateY(-5px);
  box-shadow: 0 8px 20px rgba(0,0,0,0.18);
}

/* Boxes */
  .box {
    border-radius: 10px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.12);
    border-top: 3px solid var(--primary);
    margin-bottom: 25px;
    transition: all 0.3s ease;
  }

.box:hover {
  box-shadow: 0 6px 16px rgba(0,0,0,0.15);
}

.box-header {
  background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%);
  color: white;
  border-radius: 10px 10px 0 0;
  padding: 15px 20px;
  font-weight: 600;
}

.box-header.with-border {
  border-bottom: none;
}

.box-title {
  font-size: 18px;
  font-weight: 600;
}

.box-body {
  padding: 25px;
}

/* Tab Boxes */
  .nav-tabs-custom {
    border-radius: 10px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.12);
  }

.nav-tabs-custom > .nav-tabs > li.active {
  border-top-color: var(--primary);
}

.nav-tabs-custom > .nav-tabs > li.active > a {
  border-top-color: var(--primary);
  font-weight: 600;
}

/* Buttons */
  .btn-primary {
    background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%);
    border: none;
    border-radius: 6px;
    padding: 10px 25px;
    font-weight: 600;
    transition: all 0.3s ease;
    box-shadow: 0 4px 12px rgba(46, 134, 171, 0.25);
  }

.btn-primary:hover {
  transform: translateY(-3px);
  box-shadow: 0 8px 20px rgba(46, 134, 171, 0.35);
}

.btn-success {
  background: linear-gradient(135deg, var(--success) 0%, #048661 100%);
  border: none;
  border-radius: 6px;
}

.btn-danger {
  background: linear-gradient(135deg, var(--danger) 0%, #b71c1c 100%);
  border: none;
  border-radius: 6px;
}

.btn-warning {
  background: linear-gradient(135deg, var(--accent) 0%, #d67500 100%);
  border: none;
  border-radius: 6px;
}

/* Form Controls */
  .form-control {
    border: 2px solid #DEE2E6;
    border-radius: 6px;
    padding: 10px 14px;
    transition: all 0.3s ease;
  }

.form-control:focus {
  border-color: var(--primary);
  box-shadow: 0 0 0 4px rgba(46, 134, 171, 0.1);
}

/* Select Inputs */
  .selectize-input {
    border: 2px solid #DEE2E6;
    border-radius: 6px;
    padding: 8px 12px;
  }

.selectize-input.focus {
  border-color: var(--primary);
  box-shadow: 0 0 0 4px rgba(46, 134, 171, 0.1);
}

/* Sliders */
  .irs-bar {
    background: linear-gradient(to right, var(--primary), var(--secondary));
    height: 10px;
    border-radius: 5px;
  }

.irs-handle {
  border: 4px solid var(--primary);
  background: white;
  width: 24px;
  height: 24px;
  cursor: pointer;
  border-radius: 50%;
  box-shadow: 0 3px 8px rgba(0,0,0,0.25);
}

.irs-from, .irs-to, .irs-single {
  background: var(--primary);
  border-radius: 5px;
  font-weight: 500;
}

.irs-line {
  background: #DEE2E6;
    height: 10px;
  border-radius: 5px;
}

/* Data Tables */
  .dataTables_wrapper {
    padding: 20px;
  }

table.dataTable thead th {
  background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%);
  color: white;
  font-weight: 600;
  padding: 14px 16px;
  border: none;
}

table.dataTable tbody tr:hover {
  background-color: rgba(46, 134, 171, 0.04);
}

.data-table-title {
  font-weight: bold;
  background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%);
  color: white;
}

.section-header td {
  font-weight: bold;
  background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%);
  color: white;
}

/* Loading Overlay */
  .loading-overlay {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: linear-gradient(135deg, var(--primary) 0%, var(--secondary) 100%);
    z-index: 9999;
    display: flex;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    color: white;
  }

.loading-spinner {
  border: 5px solid rgba(255,255,255,0.2);
  border-top: 5px solid white;
  border-radius: 50%;
  width: 80px;
  height: 80px;
  animation: spin 1s linear infinite;
}

@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}

/* Logo Container */
  .logo-row {
    display: flex;
    justify-content: flex-start;   /* align logos to the left */
      align-items: center;
    gap: 0.4px; #30px;                     /* more space between logos */
    flex-wrap: wrap;
    margin-right: 30px;
  }

.logo-row img {
  height: 137px; #120px;                 /* bigger default size */
  width: auto;
  transition: all 0.3s ease;
  cursor: pointer;
  padding: 8px;
  border-radius: 8px;
  background: transparent; #white;       /* blend with hero background */
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
}

.logo-row img:hover {
  transform: translateY(-5px) scale(1.05);
  box-shadow: 0 4px 16px rgba(0,0,0,0.15);
}

/* Responsive adjustments */
  @media (max-width: 992px) {
    .logo-row {
      justify-content: center;     /* center logos on medium screens */
    }
    .logo-row img {
      height: 90px;                /* slightly smaller for tablets */
    }
  }

@media (max-width: 768px) {
  .logo-row {
    flex-direction: column;      /* stack logos vertically */
      align-items: center;
    gap: 20px;
  }
  .logo-row img {
    height: 80px;                /* preserve visibility on phones */
  }
}


/* GLOBAL FONT + BODY STYLING */
  body, html, .content-wrapper, .skin-blue, .main-sidebar, .wrapper {
    font-family: 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
    #background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%) !important;
    #color: var(--text-dark, #222) !important;
    #line-height: 1.6;
    #margin: 0;
    #padding: 0;
    #overflow-x: hidden;
  }
  
"
