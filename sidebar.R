argonSidebar <- argonDashSidebar(
  vertical = TRUE,
  skin = "light",
  background = "white",
  size = "lg",
  side = "right",
  id = "my_sidebar",
  brand_url = "http://www.google.com",
  brand_logo = "https://pbs.twimg.com/profile_images/1191981430689144834/Em5N9JbQ_400x400.jpg",
  argonSidebarHeader(title = "Main Menu"),
  argonSidebarMenu(
    # argonSidebarItem(
    # tabName = "home",
    #  icon = argonIcon(name = "tv-2", color = "info"),
    #  "Home"
    # ),
    argonSidebarItem(
      tabName = "pamBaru",
      icon = argonIcon(name = "bullet-list-67", color = "danger"),
      "Membuat PAM Baru"
    ),
    argonSidebarItem(
    tabName = "modul1",
     icon = argonIcon(name = "tv-2", color = "info"),
     "Database Profitability"
    ),
    # argonSidebarItem(
    #   tabName = "template",
    #   icon = argonIcon(name = "atom", color = "danger"),
    #   "Membangun PAM dari Data Template"
    # ),
    argonSidebarItem(
      tabName = "pamParsial",
      icon = argonIcon(name = "atom", color = "danger"),
      "Simulasi Parsial Data Template"
    ),
    
    # argonSidebarItem(
    #   tabName = "deskriptifPlot",
    #   icon = argonIcon(name = "atom", color = "danger"),
    #   "Kumpulan Analisis PAM"
    # )
  ),
  argonSidebarDivider()
  #argonSidebarHeader(title = "Other Items")
)