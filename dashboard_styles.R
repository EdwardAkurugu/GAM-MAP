
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
  
  
  
  /* ============================================
   REGIONAL SUMMARY — KPI CARDS
   ============================================ */
.summary-kpi-card {
  background: #ffffff;
  border: 0.5px solid rgba(0,0,0,0.08);
  border-radius: 12px;
  padding: 18px 14px 14px;
  margin-bottom: 14px;
  text-align: center;
  box-shadow: 0 1px 4px rgba(0,0,0,0.05);
  transition: box-shadow 0.2s ease, transform 0.2s ease;
}
.summary-kpi-card:hover {
  box-shadow: 0 5px 16px rgba(0,0,0,0.10);
  transform: translateY(-2px);
}
.kpi-icon {
  width: 46px; height: 46px; border-radius: 50%;
  display: flex; align-items: center; justify-content: center;
  margin: 0 auto 10px; font-size: 20px;
}
.kpi-icon-purple { background: #EEEDFE; color: #3C3489; }
.kpi-icon-teal   { background: #E1F5EE; color: #0F6E56; }
.kpi-icon-blue   { background: #E6F1FB; color: #185FA5; }
.kpi-icon-amber  { background: #FAEEDA; color: #854F0B; }
.kpi-value {
  font-size: 22px; font-weight: 600;
  color: #1a1a2e; line-height: 1.1; margin-bottom: 4px;
}
.kpi-label {
  font-size: 18px; font-weight: 600;
  color: #534AB7; margin-top: 4px;
}
.kpi-sub {
  font-size: 16px; color: #888; margin-top: 3px;
}
 
/* ============================================
   REGIONAL SUMMARY — SECTION CARDS
   ============================================ */
.summary-section-card {
  background: #ffffff;
  border: 0.5px solid rgba(0,0,0,0.08);
  border-radius: 12px;
  padding: 18px 20px 16px;
  margin-bottom: 14px;
  box-shadow: 0 1px 4px rgba(0,0,0,0.05);
}
.summary-section-card h4 {
  font-size: 20px !important;
  font-weight: 600; margin: 0 0 2px 0;
  color: #3c3489;
  display: flex; align-items: center; gap: 8px;
}
.summary-section-card hr {
  margin: 10px 0 14px;
  border: none; border-top: 0.5px solid rgba(0,0,0,0.08);
}
 
/* Trend chip */
.trend-chip {
  display: inline-flex; align-items: center; gap: 6px;
  padding: 5px 12px; border-radius: 20px;
  font-size: 13px; font-weight: 600; margin-bottom: 10px;
}
.trend-chip-up     { background: #FCEBEB; color: #A32D2D; }
.trend-chip-down   { background: #EAF3DE; color: #3B6D11; }
.trend-chip-stable { background: #F1EFE8; color: #5F5E5A; }
 
/* Info rows inside section cards */
.summary-info-row {
  display: flex; justify-content: space-between; align-items: center;
  padding: 6px 0;
  border-bottom: 0.5px solid rgba(0,0,0,0.06);
  font-size: 14px;
}
.summary-info-row:last-child { border-bottom: none; }
.summary-info-key { color: #666; }
.summary-info-val { font-weight: 600; color: #1a1a2e; }
.summary-tick-yes { color: #0F6E56; }
.summary-tick-no  { color: #A32D2D; }
 
/* GAM stat pills (Adj R², DevExp, GCV) */
.gam-stat-trio {
  display: grid; grid-template-columns: repeat(3, 1fr);
  gap: 8px; margin-bottom: 16px;
}
.gam-stat-pill {
  background: #F4F6F9; border-radius: 8px;
  padding: 12px 8px; text-align: center;
  border: 0.5px solid rgba(0,0,0,0.06);
}
.gam-stat-val {
  font-size: 22px; font-weight: 600; color: #1a1a2e; line-height: 1.1;
}
.gam-stat-lbl {
  font-size: 13px; color: #666; margin-top: 3px;
}
.gam-model-grid {
  display: grid; grid-template-columns: 1fr 1fr; gap: 16px;
}
.gam-section-label {
  font-size: 13px; font-weight: 600; color: #888;
  text-transform: uppercase; letter-spacing: 0.05em;
  margin-bottom: 8px;
}
.gam-model-num-badge {
  display: inline-flex; align-items: center; justify-content: center;
  width: 28px; height: 28px; border-radius: 50%;
  background: #EEEDFE; color: #3C3489;
  font-size: 12px; font-weight: 600; margin-left: auto;
}
 
/* Term badges */
.summary-badge {
  display: inline-flex; align-items: center; gap: 4px;
  padding: 3px 9px; border-radius: 12px;
  font-size: 13px; font-weight: 600; letter-spacing: 0.01em;
}
.badge-blue   { background: #E6F1FB; color: #0C447C; }
.badge-teal   { background: #E1F5EE; color: #085041; }
.badge-amber  { background: #FAEEDA; color: #633806; }
.badge-purple { background: #EEEDFE; color: #26215C; }
.badge-coral  { background: #FAECE7; color: #4A1B0C; }
 
/* Narrative card */
.summary-narrative-card {
  background: #ffffff;
  border: 0.5px solid rgba(0,0,0,0.08);
  border-left: 3px solid #7F77DD;
  border-radius: 12px;
  padding: 18px 20px 16px;
  margin-bottom: 14px;
  box-shadow: 0 1px 4px rgba(0,0,0,0.05);
}
.summary-narrative-card h4 {
  font-size: 20px !important; font-weight: 600;
  color: #3c3489; margin: 0 0 2px 0;
  display: flex; align-items: center; gap: 8px;
}
.summary-narrative-card hr {
  margin: 10px 0 14px;
  border: none; border-top: 0.5px solid rgba(0,0,0,0.08);
}
.summary-narrative-text {
  font-size: 14px; line-height: 1.8;
  color: #444; text-align: justify;
}
.narrative-highlight {
  font-weight: 600; color: #1a1a2e;
}
.narrative-trend-up   { font-weight: 600; color: #A32D2D; }
.narrative-trend-down { font-weight: 600; color: #3B6D11; }
.narrative-trend-stable { font-weight: 600; color: #5F5E5A; }
 
"
