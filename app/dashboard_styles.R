
#================================================================
# GAM(M)-MAP CUSTOM CSS
#================================================================

dashboardCSS <- "

/* ============================================================
   GAM-MAP DASHBOARD CSS
   ============================================================ */

/* ---- CSS Custom Properties ---- */
:root {
  --primary:        #2E86AB;
  --primary-dark:   #1A5F7A;
  --secondary:      #A23B72;
  --accent:         #F18F01;
  --success:        #06A77D;
  --danger:         #D62828;
  --sidebar-bg:     #1E2A38;
  --content-bg:     #F4F6F9;
  --header-h:       50px;
  --sidebar-w:      250px;
  --tab-bar-h:      44px;
}

/* ==============================================================
   GLOBAL TYPOGRAPHY & BODY
   ============================================================== */
body, html, .content-wrapper, .skin-purple, .main-sidebar, .wrapper,
p, li, td, th, label, .box-title, h1, h2, h3, h4, h5, h6 {
  font-family: 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
}

.content-wrapper {
  background-color: var(--content-bg) !important;
}

/* ==============================================================
   HEADER
   ============================================================== */
.skin-purple .main-header .navbar,
.skin-purple .main-header .logo {
  background: linear-gradient(135deg, var(--primary) 0%, var(--secondary) 100%) !important;
  color: white !important;
}
.skin-purple .main-header .logo:hover {
  background: linear-gradient(135deg, var(--primary-dark) 0%, #7a2a54 100%) !important;
}
.skin-purple .main-header .navbar {
  border-bottom: 3px solid rgba(255,255,255,0.15) !important;
}
.header-action-btn {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  background: rgba(255,255,255,0.18);
  color: white !important;
  border: 1.5px solid rgba(255,255,255,0.45);
  border-radius: 6px;
  padding: 6px 14px;
  font-size: 20px;
  font-weight: 600;
  cursor: pointer;
  transition: background 0.2s ease, transform 0.15s ease;
  text-decoration: none !important;
}
.header-action-btn:hover {
  background: rgba(255,255,255,0.30);
  transform: translateY(-1px);
  color: white !important;
}

/* ==============================================================
   HEADER/LOGO/TOGGLE HEIGHT  
   ============================================================== */
.main-header {
  height: 66px !important;
}
.main-header .logo {
  height: 66px !important;
  line-height: 66px !important;
  padding: 0 15px !important;
}
.main-header .logo span {
  margin-top: 0 !important;   /* remove the old 20px offset that was stretching the box */
  line-height: 66px !important;
  vertical-align: middle;
}
.main-header .navbar {
  min-height: 66px !important;
}
.main-header .sidebar-toggle {
  height: 66px !important;
  line-height: 66px !important;
  padding: 0 15px !important;
}
.main-header .navbar > .sidebar-toggle {
  background: transparent !important;  /* let the gradient show through instead of AdminLTE's dark default */
}

/* Keep sidebar/content aligned to the new header height */
.main-sidebar, .left-side {
  padding-top: 66px !important;
}


/* ==============================================================
   SIDEBAR
   ============================================================== */
.main-sidebar, .left-side {
  background-color: var(--sidebar-bg) !important;
  box-shadow: 2px 0 12px rgba(0,0,0,0.18) !important;
}
.skin-purple .sidebar-menu > li > a {
  color: rgba(255,255,255,0.78) !important;
  border-left: 3px solid transparent;
  transition: background 0.2s ease, border-color 0.2s ease, color 0.2s ease;
  padding: 10px 15px 10px 20px;
  font-size: 20px;
  font-weight: 500;
}
.skin-purple .sidebar-menu > li:hover > a,
.skin-purple .sidebar-menu > li.active > a {
  color: white !important;
  background: rgba(241,143,1,0.14) !important;
  border-left-color: var(--accent) !important;
}
.skin-purple .sidebar-menu > li.active > a {
  background: rgba(241,143,1,0.22) !important;
}
.skin-purple .sidebar-menu .treeview-menu > li > a {
  color: rgba(255,255,255,0.65) !important;
  padding-left: 32px;
  font-size: 13px;
}
.skin-purple .sidebar-menu .treeview-menu > li.active > a,
.skin-purple .sidebar-menu .treeview-menu > li:hover > a {
  color: white !important;
  background: rgba(255,255,255,0.07) !important;
}
.sidebar-step-badge {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 22px;
  height: 22px;
  border-radius: 50%;
  background: var(--accent);
  color: #fff;
  font-size: 11px;
  font-weight: 700;
  margin-right: 8px;
  flex-shrink: 0;
}

/* ==============================================================
   SECTION TAB BAR
   ============================================================== */
.section-tab-bar {
  display: flex;
  align-items: stretch;
  background: #fff;
  border-bottom: 2px solid #dee2e6;
  margin: -20px -15px 20px -15px;
  padding: 0 10px;
  position: sticky;
  top: 0;
  z-index: 100;
  box-shadow: 0 2px 8px rgba(0,0,0,0.06);
}
.section-tab-bar .stab {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 0 18px;
  height: var(--tab-bar-h);
  font-size: 18px;
  font-weight: 600;
  color: #555;
  cursor: pointer;
  border-bottom: 3px solid transparent;
  margin-bottom: -2px;
  transition: color 0.18s, border-color 0.18s, background 0.18s;
  white-space: nowrap;
  text-decoration: none !important;
}
.section-tab-bar .stab i { font-size: 15px; }
.section-tab-bar .stab:hover {
  color: var(--primary);
  background: rgba(46,134,171,0.06);
}
.section-tab-bar .stab.active {
  color: var(--primary);
  border-bottom-color: var(--primary);
  background: rgba(46,134,171,0.05);
}
.stab-panel {
  display: none;
  font-size: 15px;
  animation: fadeInPanel 0.22s ease;
}
.stab-panel.active {
  display: block;
  font-size: 15px;
}
@keyframes fadeInPanel {
  from { opacity: 0; transform: translateY(4px); }
  to   { opacity: 1; transform: translateY(0); }
}

/* ==============================================================
   PAGE SECTION HEADERS
   ============================================================== */
.page-header-band {
  background: linear-gradient(135deg, var(--primary) 0%, var(--secondary) 100%);
  color: white;
  padding: 22px 28px;
  border-radius: 10px;
  margin-bottom: 5px;
  margin-top: 5px;
  display: flex;
  align-items: center;
  gap: 14px;
}
.page-header-band h2 {
  margin: 0;
  font-size: 3rem;
  font-weight: 600;
  color: white !important;
}
.page-header-band p {
  margin: 4px 0 0;
  opacity: 0.88;
  font-size: 4rem;
}

/* ==============================================================
   CONTENT CARDS
   ============================================================== */
.content-card {
  background: #fff;
  font-size: 15px;
  border-radius: 10px;
  box-shadow: 0 1px 6px rgba(0,0,0,0.07);
  border: 0.5px solid rgba(0,0,0,0.07);
  margin-bottom: 22px;
  overflow: hidden;
  transition: box-shadow 0.2s ease;
}
.content-card:hover { box-shadow: 0 4px 16px rgba(0,0,0,0.10); }
.content-card-header {
  display: flex;
  align-items: center;
  gap: 10px;
  padding: 14px 20px;
  border-bottom: 1px solid rgba(0,0,0,0.07);
  font-size: 16px;
  font-weight: 600;
  color: #1a1a2e;
  background: #fafbfc;
}
.content-card-header i { color: var(--primary); font-size: 17px; }
.content-card-body { padding: 20px; font-size: 15px; }

.card-accent-primary { border-top: 3px solid var(--primary); }
.card-accent-info    { border-top: 3px solid #17a2b8; }
.card-accent-success { border-top: 3px solid var(--success); }
.card-accent-warning { border-top: 3px solid var(--accent); }
.card-accent-purple  { border-top: 3px solid var(--secondary); }

.box {
  border-radius: 10px !important;
  box-shadow: 0 1px 6px rgba(0,0,0,0.07) !important;
  border-top: 3px solid var(--primary) !important;
  margin-bottom: 22px !important;
}
.box:hover { box-shadow: 0 4px 14px rgba(0,0,0,0.11) !important; }
.box-header {
  background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%) !important;
  color: white !important;
  border-radius: 10px 10px 0 0 !important;
  padding: 13px 18px !important;
}
.box-title { font-size: 15px !important; font-weight: 600 !important; }
.box-body  { padding: 20px !important; }
.nav-tabs-custom { border-radius: 10px !important; box-shadow: 0 1px 6px rgba(0,0,0,0.07) !important; }
.nav-tabs-custom > .nav-tabs > li.active { border-top-color: var(--primary) !important; }

/* ==============================================================
   FILTER PANEL
   ============================================================== */
.filter-panel {
  background: #fff;
  font-size: 15px;
  border-radius: 10px;
  box-shadow: 0 1px 6px rgba(0,0,0,0.07);
  border: 0.5px solid rgba(0,0,0,0.07);
  border-top: 3px solid var(--primary);
  padding: 18px 16px;
  margin-bottom: 22px;
}
.filter-panel-title {
  font-size: 15px;
  font-weight: 700;
  color: #1a1a2e;
  margin-bottom: 16px;
  display: flex;
  align-items: center;
  gap: 8px;
  border-bottom: 1px solid rgba(0,0,0,0.08);
  padding-bottom: 10px;
}
.filter-panel-title i { color: var(--primary); }

/* ==============================================================
   NAVIGATION BUTTONS
   ============================================================== */
.nav-next-btn {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%);
  color: white !important;
  border: none;
  border-radius: 7px;
  padding: 10px 28px;
  font-size: 14px;
  font-weight: 700;
  letter-spacing: 0.05em;
  text-transform: uppercase;
  cursor: pointer;
  transition: all 0.2s ease;
  box-shadow: 0 3px 10px rgba(46,134,171,0.30);
}
.nav-next-btn:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 16px rgba(46,134,171,0.38);
  color: white !important;
}
.nav-next-btn i { font-size: 16px; }

/* ==============================================================
   HERO / WELCOME BAND
   ============================================================== */
.hero-band {
  padding: 10px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  background: linear-gradient(135deg, var(--content-bg) 0%, var(--primary) 100%) !important;
  border-radius: 10px;
}
.logo-row {
  display: flex;
  justify-content: flex-start;
  align-items: center;
  gap: 0.4px;
  flex-wrap: wrap;
  margin-right: 30px;
}
.logo-row img {
  height: 137px;
  width: auto;
  transition: all 0.3s ease;
  cursor: pointer;
  padding: 8px;
  border-radius: 8px;
  background: transparent;
  box-shadow: 0 2px 8px rgba(0,0,0,0.08);
}
.logo-row img:hover {
  transform: translateY(-5px) scale(1.05);
  box-shadow: 0 4px 16px rgba(0,0,0,0.15);
}
.logo-gammap svg { width: 170px !important; height: 150px !important; }

@media (max-width: 992px) { .logo-row { justify-content: center; } .logo-row img { height: 90px; } }
@media (max-width: 768px) { .logo-row { flex-direction: column; align-items: center; gap: 20px; } .logo-row img { height: 80px; } }

/* ==============================================================
   INFO / VALUE BOXES
   ============================================================== */
.info-box {
  border-radius: 10px !important;
  box-shadow: 0 2px 10px rgba(0,0,0,0.09) !important;
  transition: all 0.25s ease !important;
  border-top: 3px solid !important;
}
.info-box:hover { transform: translateY(-4px) !important; box-shadow: 0 6px 18px rgba(0,0,0,0.14) !important; }
.info-box-icon  { border-radius: 10px 0 0 10px !important; }
.small-box      { border-radius: 10px !important; box-shadow: 0 2px 10px rgba(0,0,0,0.09) !important; }
.small-box:hover{ transform: translateY(-4px) !important; box-shadow: 0 6px 18px rgba(0,0,0,0.14) !important; }

/* ==============================================================
   FORM CONTROLS
   ============================================================== */
.form-control {
  border: 1.5px solid #dee2e6 !important;
  border-radius: 6px !important;
  padding: 9px 13px !important;
  transition: all 0.25s ease !important;
  font-size: 13.5px !important;
}
.form-control:focus {
  border-color: var(--primary) !important;
  box-shadow: 0 0 0 3px rgba(46,134,171,0.12) !important;
}
.selectize-input { border: 1.5px solid #dee2e6 !important; border-radius: 6px !important; }
.selectize-input.focus {
  border-color: var(--primary) !important;
  box-shadow: 0 0 0 3px rgba(46,134,171,0.12) !important;
}
.irs-bar     { background: linear-gradient(to right, var(--primary), var(--secondary)) !important; height: 9px !important; border-radius: 5px !important; }
.irs-handle  { border: 3px solid var(--primary) !important; background: white !important; border-radius: 50% !important; box-shadow: 0 2px 6px rgba(0,0,0,0.2) !important; }
.irs-from, .irs-to, .irs-single { background: var(--primary) !important; border-radius: 4px !important; }
.irs-line    { background: #dee2e6 !important; height: 9px !important; border-radius: 5px !important; }

/* ==============================================================
   BUTTONS
   ============================================================== */
.btn-primary {
  background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%) !important;
  border: none !important; border-radius: 6px !important; font-weight: 600 !important;
  transition: all 0.25s ease !important; box-shadow: 0 3px 10px rgba(46,134,171,0.22) !important;
}
.btn-primary:hover { transform: translateY(-2px) !important; box-shadow: 0 6px 16px rgba(46,134,171,0.32) !important; }
.btn-success { background: linear-gradient(135deg, var(--success) 0%, #048661 100%) !important; border: none !important; border-radius: 6px !important; }
.btn-danger  { background: linear-gradient(135deg, var(--danger)  0%, #b71c1c 100%) !important; border: none !important; border-radius: 6px !important; }
.btn-warning { background: linear-gradient(135deg, var(--accent)  0%, #d67500 100%) !important; border: none !important; border-radius: 6px !important; }
.btn-default { border-radius: 6px !important; border: 1.5px solid #ced4da !important; font-weight: 600 !important; transition: all 0.2s ease !important; }
.btn-default:hover { background: #f8f9fa !important; border-color: var(--primary) !important; }

/* ==============================================================
   DATA TABLES
   ============================================================== */
.dataTables_wrapper { padding: 16px !important; }
table.dataTable thead th {
  background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%) !important;
  color: white !important;
  font-weight: 600 !important;
  padding: 12px 15px !important;
  border: none !important;
  font-size: 16px !important;
}
table.dataTable tbody tr:hover { background-color: rgba(46,134,171,0.04) !important; }
.data-table-title {
  font-weight: bold !important;
  font-size: 16px !important;
  background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%) !important;
  color: white !important;
}
.section-header td {
  font-weight: bold !important;
  background: linear-gradient(135deg, var(--primary) 0%, var(--primary-dark) 100%) !important;
  color: white !important;
}

/* ==============================================================
   LOADING OVERLAY
   ============================================================== */
.loading-overlay {
  position: fixed; top: 0; left: 0; right: 0; bottom: 0;
  background: linear-gradient(135deg, var(--primary) 0%, var(--secondary) 100%);
  z-index: 9999;
  display: flex; flex-direction: column; justify-content: center; align-items: center;
  color: white;
}
.loading-spinner {
  border: 5px solid rgba(255,255,255,0.2);
  border-top: 5px solid white;
  border-radius: 50%; width: 80px; height: 80px;
  animation: spin 1s linear infinite;
}
@keyframes spin { 0%{transform:rotate(0deg)} 100%{transform:rotate(360deg)} }

/* ==============================================================
   REGIONAL SUMMARY CARDS
   ============================================================== */
.summary-kpi-card {
  background: #ffffff; border: 0.5px solid rgba(0,0,0,0.08); border-radius: 12px;
  padding: 18px 14px 14px; margin-bottom: 14px; text-align: center;
  box-shadow: 0 1px 4px rgba(0,0,0,0.05); transition: box-shadow 0.2s ease, transform 0.2s ease;
}
.summary-kpi-card:hover { box-shadow: 0 5px 16px rgba(0,0,0,0.10); transform: translateY(-2px); }
.kpi-icon { width:46px;height:46px;border-radius:50%;display:flex;align-items:center;justify-content:center;margin:0 auto 10px;font-size:20px; }
.kpi-icon-purple { background:#EEEDFE; color:#3C3489; }
.kpi-icon-teal   { background:#E1F5EE; color:#0F6E56; }
.kpi-icon-blue   { background:#E6F1FB; color:#185FA5; }
.kpi-icon-amber  { background:#FAEEDA; color:#854F0B; }
.kpi-value { font-size:22px; font-weight:600; color:#1a1a2e; line-height:1.1; margin-bottom:4px; }
.kpi-label { font-size:18px; font-weight:600; color:#534AB7; margin-top:4px; }
.kpi-sub   { font-size:16px; color:#888; margin-top:3px; }

.summary-section-card {
  background:#ffffff; border:0.5px solid rgba(0,0,0,0.08); border-radius:12px;
  padding:18px 20px 16px; margin-bottom:14px; box-shadow:0 1px 4px rgba(0,0,0,0.05);
}
.summary-section-card h4 { font-size:20px !important; font-weight:600; margin:0 0 2px 0; color:#3c3489; display:flex; align-items:center; gap:8px; }
.summary-section-card hr { margin:10px 0 14px; border:none; border-top:0.5px solid rgba(0,0,0,0.08); }

.trend-chip { display:inline-flex;align-items:center;gap:6px;padding:5px 12px;border-radius:20px;font-size:13px;font-weight:600;margin-bottom:10px; }
.trend-chip-up     { background:#FCEBEB; color:#A32D2D; }
.trend-chip-down   { background:#EAF3DE; color:#3B6D11; }
.trend-chip-stable { background:#F1EFE8; color:#5F5E5A; }

.summary-info-row { display:flex;justify-content:space-between;align-items:center;padding:6px 0;border-bottom:0.5px solid rgba(0,0,0,0.06);font-size:14px; }
.summary-info-row:last-child { border-bottom:none; }
.summary-info-key { color:#666; }
.summary-info-val { font-weight:600; color:#1a1a2e; }
.summary-tick-yes { color:#0F6E56; }
.summary-tick-no  { color:#A32D2D; }

.gam-stat-trio { display:grid; grid-template-columns:repeat(3,1fr); gap:8px; margin-bottom:16px; }
.gam-stat-pill { background:#F4F6F9; border-radius:8px; padding:12px 8px; text-align:center; border:0.5px solid rgba(0,0,0,0.06); }
.gam-stat-val  { font-size:22px; font-weight:600; color:#1a1a2e; line-height:1.1; }
.gam-stat-lbl  { font-size:13px; color:#666; margin-top:3px; }
.gam-model-grid { display:grid; grid-template-columns:1fr 1fr; gap:16px; }
.gam-section-label { font-size:13px; font-weight:600; color:#888; text-transform:uppercase; letter-spacing:0.05em; margin-bottom:8px; }
.gam-model-num-badge { display:inline-flex;align-items:center;justify-content:center;width:28px;height:28px;border-radius:50%;background:#EEEDFE;color:#3C3489;font-size:12px;font-weight:600;margin-left:auto; }

.summary-badge  { display:inline-flex;align-items:center;gap:4px;padding:3px 9px;border-radius:12px;font-size:13px;font-weight:600; }
.badge-blue     { background:#E6F1FB; color:#0C447C; }
.badge-teal     { background:#E1F5EE; color:#085041; }
.badge-amber    { background:#FAEEDA; color:#633806; }
.badge-purple   { background:#EEEDFE; color:#26215C; }
.badge-coral    { background:#FAECE7; color:#4A1B0C; }

.summary-narrative-card {
  background:#ffffff; border:0.5px solid rgba(0,0,0,0.08); border-left:3px solid #7F77DD;
  border-radius:12px; padding:18px 20px 16px; margin-bottom:14px; box-shadow:0 1px 4px rgba(0,0,0,0.05);
}
.summary-narrative-card h4 { font-size:20px !important; font-weight:600; color:#3c3489; margin:0 0 2px 0; display:flex; align-items:center; gap:8px; }
.summary-narrative-card hr { margin:10px 0 14px; border:none; border-top:0.5px solid rgba(0,0,0,0.08); }
.summary-narrative-text { font-size:14px; line-height:1.8; color:#444; text-align:justify; }
.narrative-highlight     { font-weight:600; color:#1a1a2e; }
.narrative-trend-up      { font-weight:600; color:#A32D2D; }
.narrative-trend-down    { font-weight:600; color:#3B6D11; }
.narrative-trend-stable  { font-weight:600; color:#5F5E5A; }

/* ==============================================================
   INSTRUCTION CARDS
   ============================================================== */
.instruction-card {
  background: #f8fafc;
  border-left: 4px solid var(--primary);
  border-radius: 0 8px 8px 0;
  padding: 16px 20px;
  margin-bottom: 20px;
  font-size: 12px;
  line-height: 1.7;
  color: #333;
  text-align: justify;
}
.instruction-card strong { color: var(--primary-dark); }

/* ==============================================================
   NEXT STEP BANNER
   ============================================================== */
.next-step-bar {
  background: linear-gradient(90deg, var(--primary) 0%, var(--secondary) 100%);
  border-radius: 8px;
  padding: 14px 24px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-top: 24px;
  margin-bottom: 8px;
}
.next-step-bar p      { margin: 0; color: rgba(255,255,255,0.92); font-size: 14px; }
.next-step-bar strong { color: white; font-size: 15px; }

/* ==============================================================
   ANALYSIS STEP STEPPER
   ============================================================== */
.analysis-stepper {
  display: flex; gap: 0; margin-bottom: 22px; border-radius: 8px;
  overflow: hidden; border: 1px solid rgba(0,0,0,0.09);
}
.analysis-step {
  flex: 1; display: flex; align-items: center; gap: 10px; padding: 11px 16px;
  background: #fff; cursor: pointer; font-size: 16px; font-weight: 600; color: #666;
  border-right: 1px solid rgba(0,0,0,0.08);
  transition: background 0.18s, color 0.18s;
}
.analysis-step:last-child { border-right: none; }
.analysis-step .step-num {
  width: 24px; height: 24px; border-radius: 50%;
  background: #e9ecef; color: #666;
  display: flex; align-items: center; justify-content: center;
  font-size: 12px; font-weight: 700; flex-shrink: 0;
  transition: background 0.18s, color 0.18s;
}
.analysis-step:hover         { background: #f8fafc; color: var(--primary); }
.analysis-step:hover .step-num { background: rgba(46,134,171,0.15); color: var(--primary); }
.analysis-step.active        { background: linear-gradient(135deg, rgba(46,134,171,0.08), rgba(162,59,114,0.05)); color: var(--primary); }
.analysis-step.active .step-num { background: var(--primary); color: white; }
.analysis-step.done          { background: rgba(6,167,125,0.07); color: var(--success); }
.analysis-step.done .step-num   { background: var(--success); color: white; }

/* ==============================================================
   CODE PANEL
   ============================================================== */
.github-panel {
  background: linear-gradient(135deg, #24292e 0%, #000 100%);
  color: white; padding: 50px; margin-bottom: 30px;
  text-align: center; border-radius: 10px;
}
.github-panel h2 { margin: 0 0 20px; }
.github-panel p  { font-size: 1.25rem; opacity: 0.9; }

/* ==============================================================
   YOY BADGE & THRESHOLD ALERTS  
   ============================================================== */
.yoy-badge {
  display: inline-flex; align-items: center; gap: 6px;
  padding: 5px 12px; border-radius: 20px;
  font-size: 13px; font-weight: 600; margin-bottom: 10px;
}
.yoy-up     { background: #FCEBEB; color: #A32D2D; }
.yoy-down   { background: #EAF3DE; color: #3B6D11; }
.yoy-stable { background: #F1EFE8; color: #5F5E5A; }

.threshold-alert {
  padding: 10px 16px; border-radius: 8px;
  font-size: 14px; font-weight: 600;
  margin-bottom: 14px; display: flex; align-items: center; gap: 8px;
}
.alert-high   { background: #FCEBEB; color: #A32D2D; border-left: 4px solid #A32D2D; }
.alert-low    { background: #EAF3DE; color: #3B6D11; border-left: 4px solid #3B6D11; }
.alert-normal { background: #E6F1FB; color: #185FA5; border-left: 4px solid #185FA5; }

/* ==============================================================
   LEAFLET MAP
   ============================================================== */
.leaflet-container { border-radius: 10px !important; }

/* ==============================================================
   RESPONSIVE
   ============================================================== */
@media (max-width: 768px) {
  .section-tab-bar { flex-wrap: wrap; }
  .section-tab-bar .stab { padding: 0 10px; font-size: 12px; }
  .gam-model-grid { grid-template-columns: 1fr; }
  .gam-stat-trio  { grid-template-columns: 1fr 1fr; }
  .analysis-stepper { flex-wrap: wrap; }
}


"
