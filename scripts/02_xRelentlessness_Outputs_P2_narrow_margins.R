install.packages("gt")
remotes::install_github("jthomasmock/gtExtras")

pacman::p_load(gt, gtExtras)
#Win_by_1 --------
win_by_1 <- season_clean %>% 
  filter(season != "2016/17") %>% 
  select(competition, season, match_id, minute, h_team, a_team, h_goals, a_goals) %>%
  group_by(match_id) %>%
  filter(minute == max(minute)) %>% 
  distinct(match_id, .keep_all = T) %>% 
  mutate(GD = h_goals - a_goals,
         team = ifelse(GD > 0, h_team, ifelse(GD < 0, a_team, "draw"))) %>% 
  rbind(lei1516_gd) %>% 
  mutate(by_1 = ifelse(abs(GD) == 1, 1, 0)) %>% 
  filter(GD != 0) %>%
  group_by(team, season) %>% 
  summarise(across(where(is.numeric), ~ sum(.x))) %>%
  arrange(desc(by_1)) %>% 
  select(team, season, by_1) %>% 
  mutate(joiner = sub(" ","_",paste0(team,season))) %>% 
  ungroup()

win_by_1

#Get teams who have won with the smallest win margins -----
win_margins <- season_clean %>% 
  filter(season != "2016/17") %>% 
  select(competition, season, match_id, minute, h_team, a_team, h_goals, a_goals) %>% 
  group_by(match_id) %>%
  filter(minute == max(minute)) %>% 
  distinct(match_id, .keep_all = T) %>% 
  mutate(GD = h_goals - a_goals,
         team = ifelse(GD > 0, h_team, ifelse(GD < 0,a_team,"draw"))) %>% 
  filter(team != "draw") %>% 
  rbind(lei1516_gd) %>%
  mutate(GD = abs(GD)) %>% 
  ungroup() %>% 
  select(competition, match_id,season, team, GD) %>% 
  distinct(match_id, .keep_all = T) %>%
  group_by(team, season) %>% 
  summarise(GD = sum(GD)) %>% 
  arrange(desc(GD)) %>% 
  ungroup() %>% 
  left_join(league_tables,by = c("team", "season")) %>% 
  left_join(win_by_1, by = c("team", "season")) %>% 
  select(competition, team, season, GD, M, W, by_1, Pos) %>% 
  mutate(avg_win_margin = round(GD/W,2),
         win_percentage = W/M,
         by_one_percent = signif(by_1 * 100/ W,4)) %>% 
  arrange(avg_win_margin)

win_margins

#Liverpool ------
win_margins %>% filter(team == "Liverpool") %>% 
  select(team, season, Pos, W, by_1, by_one_percent) %>% 
  arrange(desc(by_one_percent)) %>% 
  rename(Team = team,
         Season = season,
         Wins = W,
         `Wins by 1` = by_1,
         `%` = by_one_percent,
         `League Position` = Pos) %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(badge) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:`%`)
  ) %>% 
  #set column width
  cols_width(
    badge ~ 60,
    Team ~ 100,
    `%` ~ 60,
    # c(`xGA (One Ahead)`:`Mins (One Ahead)`) ~ 100,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " "
  ) %>% 
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
    sides = "right",
    color = "black",
    weight = px(1)
  )),
  locations = list(cells_body(columns = c(Team,Season,`League Position`,`Wins by 1`)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #set font sizes and header style
  tab_options(
    data_row.padding = px(5),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))


#League Winners ----
win_margins %>% filter(Pos == 1) %>% 
  select(team, season, Pos, W, by_1, by_one_percent) %>% 
  arrange(desc(by_one_percent)) %>% 
  rename(Team = team,
         Season = season,
         Wins = W,
         `Wins by 1` = by_1,
         `%` = by_one_percent,
         `League Position` = Pos) %>% 
  slice(1:15) %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(badge) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:`%`)
  ) %>% 
  #set column width
  cols_width(
    badge ~ 60,
    Team ~ 120,
    `%` ~ 60,
    # c(`xGA (One Ahead)`:`Mins (One Ahead)`) ~ 100,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " "
  ) %>% 
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
    sides = "right",
    color = "black",
    weight = px(1)
  )),
  locations = list(cells_body(columns = c(Team,Season,`League Position`,`Wins by 1`)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #set font sizes and header style
  tab_options(
    data_row.padding = px(5),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))
  

#teams with highest xGA while ahead ----
#one ahead
pma_by_team_season %>% 
  select(2:4, total_xGA_one_ahead, one_ahead, xGA_per_one_ahead) %>% 
  mutate(total_xGA_one_ahead = round(total_xGA_one_ahead,1),
         xGA_per_one_ahead = round(xGA_per_one_ahead,4)) %>% 
  arrange(desc(xGA_per_one_ahead)) %>% 
  slice(1:10) %>% 
  rename(Team = team,
         Season = season,
         `xGA (One Ahead)` = total_xGA_one_ahead,
         `Mins (One Ahead)` = one_ahead,
         xGApMA = xGA_per_one_ahead,
         `League Position` = Pos) %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(badge) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:xGApMA)
  ) %>% 
  #set column width
  cols_width(
    badge ~ 60,
    Team ~ 180,
    c(`xGA (One Ahead)`:`Mins (One Ahead)`) ~ 100,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " "
  ) %>%
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
    sides = "right",
    color = "black",
    weight = px(1)
  )),
  locations = list(cells_body(columns = c(Team,Season,`League Position`,`Mins (One Ahead)`)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #set font sizes and header style
  tab_options(
    data_row.padding = px(5),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))

#One Ahead (Filtered)
pma_by_team_season %>% 
  filter(W/M > 0.5) %>% 
  select(2:4, total_xGA_one_ahead, one_ahead, xGA_per_one_ahead) %>% 
  mutate(total_xGA_one_ahead = round(total_xGA_one_ahead,1),
         xGA_per_one_ahead = round(xGA_per_one_ahead,4)) %>% 
  arrange(desc(xGA_per_one_ahead)) %>% 
  slice(1:10) %>% 
  rename(Team = team,
         Season = season,
         `xGA (One Ahead)` = total_xGA_one_ahead,
         `Mins (One Ahead)` = one_ahead,
         xGApMA = xGA_per_one_ahead,
         `League Position` = Pos) %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(badge) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:xGApMA)
  ) %>% 
  #set column width
  cols_width(
    badge ~ 60,
    Team ~ 180,
    c(`xGA (One Ahead)`:`Mins (One Ahead)`) ~ 100,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " "
  ) %>%
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
        sides = "right",
        color = "black",
        weight = px(1)
      )),
    locations = list(cells_body(columns = c(Team,Season,`League Position`,`Mins (One Ahead)`)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #set font sizes and header style
  tab_options(
    data_row.padding = px(5),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #insert footnote
  tab_footnote(footnote = "Only inlcudes teams who won at least half their league matches in a given season") %>% 
  tab_style(locations = cells_footnotes(),
            style = list(cell_text(size = "16px"),
                         "vertical-align:middle")) %>% 
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))
  
#normalised
pma_by_team_season %>% 
  filter(W/M > 0.5) %>% 
  select(2:4, norm_xGA_one_ahead) %>% 
  mutate(norm_xGA_one_ahead = round(norm_xGA_one_ahead,3)) %>% 
  arrange(desc(norm_xGA_one_ahead)) %>% 
  slice(1:10) %>% 
  rename(Team = team,
         Season = season,
         `xGApMA` = norm_xGA_one_ahead,
         `League Position` = Pos) %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(badge) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:xGApMA)
  ) %>% 
  #set column width
  cols_width(
    badge ~ 60,
    Team ~ 180,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " "
  ) %>%
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
    sides = "right",
    color = "black",
    weight = px(1)
  )),
  locations = list(cells_body(columns = c(Team,Season)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #set font sizes and header style
  tab_options(
    data_row.padding = px(5),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #insert footnote
  tab_footnote(footnote = "Only inlcudes teams who won at least half their league matches in a given season") %>% 
  tab_style(locations = cells_footnotes(),
            style = list(cell_text(size = "16px"),
                         "vertical-align:middle")) %>% 
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))

#xGApMA/xGpMA Ratio -------
#Leading by One
pma_xG_xGA %>% 
  mutate(against_vs_for = round(norm_xGA_one_ahead / norm_xG_one_ahead,3)) %>% 
  filter(W/M > 0.5) %>% 
  select(3,2,6,against_vs_for) %>%
  arrange(desc(against_vs_for))%>% 
  slice(1:10) %>% 
  rename(Team = team,
         Season = season,
         `xGApMA/xGpMA` = against_vs_for,
         `League Position` = Pos)  %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(badge) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:`xGApMA/xGpMA`)
  ) %>% 
  #set column width
  cols_width(
    badge ~ 60,
    Team ~ 180,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " ",
    `xGApMA/xGpMA` = stringr::str_wrap("xGApMA/xGpMA",7),
  ) %>%
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
    sides = "right",
    color = "black",
    weight = px(1)
  )),
  locations = list(cells_body(columns = c(Team,Season)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #set font sizes and header style
  tab_options(
    data_row.padding = px(5),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #insert footnote 1
  tab_footnote(footnote = "Only inlcudes teams who won at least half their league matches in a given season") %>% 
  tab_style(locations = cells_footnotes(),
            style = list(cell_text(size = "16px"),
                         "vertical-align:middle")) %>% 
  #insert footnote 2
  tab_footnote(footnote = "xGA and xG per minute ahead while leading by one goal") %>% 
  tab_style(locations = cells_footnotes(),
            style = list(cell_text(size = "16px"),
                         "vertical-align:middle")) %>%
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))
 
#Leading by One (Atletico Madrid)
pma_xG_xGA %>% 
  mutate(against_vs_for = round(norm_xGA_one_ahead / norm_xG_one_ahead,3)) %>% 
  filter(W/M > 0.5 & competition == "La Liga" & season == "2020/21") %>% 
  select(3,2,6,against_vs_for) %>%
  arrange(desc(against_vs_for))%>% 
  slice(1:10) %>% 
  rename(Team = team,
         Season = season,
         `xGApMA/xGpMA` = against_vs_for,
         `League Position` = Pos)  %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(badge) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:`xGApMA/xGpMA`)
  ) %>% 
  #set column width
  cols_width(
    badge ~ 60,
    Team ~ 180,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " ",
    `xGApMA/xGpMA` = stringr::str_wrap("xGApMA/xGpMA",7),
  ) %>%
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
    sides = "right",
    color = "black",
    weight = px(1)
  )),
  locations = list(cells_body(columns = c(Team,Season)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #set font sizes and header style
  tab_options(
    data_row.padding = px(5),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #insert footnote 1
  tab_footnote(footnote = "Only inlcudes teams who won at least half their league matches in a given season") %>% 
  tab_style(locations = cells_footnotes(),
            style = list(cell_text(size = "16px"),
                         "vertical-align:middle")) %>% 
  #insert footnote 2
  tab_footnote(footnote = "xGA and xG per minute ahead while leading by one goal") %>% 
  tab_style(locations = cells_footnotes(),
            style = list(cell_text(size = "16px"),
                         "vertical-align:middle")) %>%
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))

#Leading by > 1 (Unweighted)
pma_xG_xGA %>% 
  mutate(against_vs_for = round(norm_xGApMA / norm_xGpMA,3)) %>% 
  filter(W/M > 0.5) %>% 
  select(3,2,6,against_vs_for) %>%
  arrange(desc(against_vs_for))%>% 
  slice(1:10) %>% 
  rename(Team = team,
         Season = season,
         # `xGApMA` = norm_xGApMA,
         `xGApMA/xGpMA` = against_vs_for,
         # `% Diff` = percent_diff_against,
         `League Position` = Pos) %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(badge) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:`xGApMA/xGpMA`)
  ) %>% 
  #set column width
  cols_width(
    badge ~ 60,
    Team ~ 180,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " ",
    `xGApMA/xGpMA` = stringr::str_wrap("xGApMA/xGpMA",7),
  ) %>%
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
    sides = "right",
    color = "black",
    weight = px(1)
  )),
  locations = list(cells_body(columns = c(Team,Season)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #set font sizes and header style
  tab_options(
    data_row.padding = px(5),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #insert footnote 1
  tab_footnote(footnote = "Only inlcudes teams who won at least half their league matches in a given season") %>% 
  tab_style(locations = cells_footnotes(),
            style = list(cell_text(size = "16px"),
                         "vertical-align:middle")) %>% 
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))

#Leading by > 1 (Weighted)
pma_xG_xGA %>% 
  mutate(against_vs_for = round(norm_xGApMA_wt / norm_xGpMA_wt,3)) %>% 
  filter(W/M > 0.5) %>% 
  select(3,2,6,against_vs_for) %>%
  arrange(against_vs_for)%>% 
  slice(1:10) %>% 
  rename(Team = team,
         Season = season,
         # `xGApMA` = norm_xGApMA,
         `xGApMA/xGpMA` = against_vs_for,
         # `% Diff` = percent_diff_against,
         `League Position` = Pos) %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(badge) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:`xGApMA/xGpMA`)
  ) %>% 
  #set column width
  cols_width(
    badge ~ 60,
    Team ~ 180,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " ",
    `xGApMA/xGpMA` = stringr::str_wrap("xGApMA/xGpMA (Weighted)",7),
  ) %>%
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
    sides = "right",
    color = "black",
    weight = px(1)
  )),
  locations = list(cells_body(columns = c(Team,Season)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #set font sizes and header style
  tab_options(
    data_row.padding = px(5),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #insert footnote 1
  tab_footnote(footnote = "Only inlcudes teams who won at least half their league matches in a given season") %>% 
  tab_style(locations = cells_footnotes(),
            style = list(cell_text(size = "16px"),
                         "vertical-align:middle")) %>% 
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))

#Leading by > 1 (Weighted) multiplied Win Percentage
pma_xG_xGA %>% 
  mutate(against_vs_for = round(norm_xGApMA_wt / norm_xGpMA_wt,3),
         win_percentage = round(W/M,3),
         agfor_win = (W/M)*against_vs_for) %>% 
  filter(win_percentage > 0.5) %>% 
  select(3,2,6,against_vs_for,win_percentage,agfor_win) %>%
  arrange(desc(agfor_win))%>% 
  slice(1:10) %>% 
  rename(Team = team,
         Season = season,
         `Win %` = win_percentage,
         `xGApMA/xGpMA` = against_vs_for,
         `Ratio x Win %` = agfor_win,
         `League Position` = Pos) %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(badge) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:last_col())
  ) %>% 
  #format numbers
  fmt_percent(columns = `Win %`, decimals = 1) %>% 
  fmt_number(columns = `Ratio x Win %`, decimals = 3) %>% 
  #set column width
  cols_width(
    badge ~ 60,
    Team ~ 180,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " ",
    `xGApMA/xGpMA` = stringr::str_wrap("xGApMA/xGpMA (Weighted)",7)
  ) %>%
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
    sides = "right",
    color = "black",
    weight = px(1)
  )),
  locations = list(cells_body(columns = c(Team,Season,`League Position`,`Win %`)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #set font sizes and header style
  tab_options(
    data_row.padding = px(5),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #insert footnote 1
  tab_footnote(footnote = "Only inlcudes teams who won at least half their league matches in a given season") %>% 
  tab_style(locations = cells_footnotes(),
            style = list(cell_text(size = "16px"),
                         "vertical-align:middle")) %>% 
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))

#Leading by > 1 (Weighted) - League Champions
narrow_margin_champs <- pma_xG_xGA %>% 
  filter(season != "2015/16") %>% 
  mutate(against_vs_for = round(norm_xGApMA_wt / norm_xGpMA_wt,3),
         win_percentage = round(W/M,3),
         agfor_win = (W/M)*against_vs_for) %>% 
  filter(Pos == 1) %>% 
  select(3,2,against_vs_for,win_percentage,agfor_win) %>%
  arrange(desc(agfor_win))%>% 
  mutate(Rk = row_number())%>% 
  # slice(1:15) %>% 
  rename(Team = team,
         Season = season,
         `Win %` = win_percentage,
         `xGApMA/xGpMA` = against_vs_for,
         `Ratio x Win %` = agfor_win) %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(Rk, badge, everything()) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:last_col())
  ) %>% 
  #format numbers
  fmt_percent(columns = `Win %`, decimals = 1) %>% 
  fmt_number(columns = `Ratio x Win %`, decimals = 3) %>% 
  #set column width
  cols_width(
    Rk ~ 25,
    badge ~ 60,
    Team ~ 180,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " ",
    `xGApMA/xGpMA` = stringr::str_wrap("xGApMA/xGpMA (Weighted)",7),
  ) %>%
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
    sides = "right",
    color = "black",
    weight = px(1)
  )),
  locations = list(cells_body(columns = c(Team,Season,`Win %`)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #highlight Inter
  tab_style(style = list(cell_fill(color = badge_set$team_colour[badge_set$team == "Inter"], alpha = .5),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 5)) %>%
  #set font sizes and header style
  tab_options(
    data_row.padding = px(1),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "20px",
    heading.subtitle.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #insert header
  tab_header(title = "Narrow Margin Champions", subtitle = "Europe's Top 5 League, 2017/18 - 2021/22") %>% 
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))

gtsave(narrow_margin_champs, filename = here::here("outputs","narrow_margin_champs.png"))


#Leading by > 1 (Weighted) - Overall
pma_xG_xGA_team %>% 
  filter(W/M > .5) %>% 
  mutate(against_vs_for = round(norm_xGApMA_wt / norm_xGpMA_wt,3),
         win_percentage = round(W/M,3),
         agfor_win = (W/M)*against_vs_for) %>% 
  select(1,against_vs_for,win_percentage,agfor_win) %>%
  arrange(desc(agfor_win))%>% 
  mutate(Rk = row_number())%>% 
  rename(Team = team,
         `Win %` = win_percentage,
         `xGApMA/xGpMA` = against_vs_for,
         `Ratio x Win %` = agfor_win) %>% 
  #join badges and move to beginning
  left_join(select(badge_set, c(team,badge)), by = c("Team" = "team")) %>% 
  relocate(Rk, badge, everything()) %>% 
  slice(1:10) %>% 
  #start building table
  gt() %>%
  #insert club badges
  gt_img_rows(columns = badge, img_source = "web", height = 20) %>% 
  cols_align(
    align = "center",
    columns = c(badge,Season:last_col())
  ) %>% 
  #format numbers
  fmt_percent(columns = `Win %`, decimals = 1) %>% 
  fmt_number(columns = `Ratio x Win %`, decimals = 3) %>% 
  #set column width
  cols_width(
    Rk ~ 25,
    badge ~ 60,
    Team ~ 180,
    everything() ~ px(80)
  ) %>% 
  #remove badge column name
  cols_label(
    badge = " ",
    `xGApMA/xGpMA` = stringr::str_wrap("xGApMA/xGpMA (Weighted)",7),
  ) %>%
  #set font and size
  opt_table_font(font = plotfont) %>% 
  tab_style(style = cell_fill(color = bgcol),
            locations = cells_body(columns = everything(), rows = everything())) %>%
  #insert right cell borders
  tab_style(style = list(cell_borders(
    sides = "right",
    color = "black",
    weight = px(1)
  )),
  locations = list(cells_body(columns = c(Team,`Win %`)))
  ) %>% 
  #set calculation background colour
  tab_style(style = cell_fill(color = "#eba6a2", alpha = 1),
            locations = cells_body(columns = last_col())) %>% 
  #set top row colour and style
  tab_style(style = list(cell_fill(color = "#8F1D16", alpha = 0.75),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 1)) %>% 
  #highlight Inter
  tab_style(style = list(cell_fill(color = badge_set$team_colour[badge_set$team == "Inter"], alpha = .5),
                         cell_text(color = bgcol, weight = "normal")),
            locations = cells_body(rows = 5)) %>%
  #set font sizes and header style
  tab_options(
    data_row.padding = px(1),
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table.font.size = "16px",
    heading.title.font.size = "20px",
    heading.subtitle.font.size = "16px",
    heading.align = "left",
    heading.background.color = textcol,
    heading.border.bottom.color = textcol
  ) %>% 
  #insert header
  tab_header(title = "Narrow Margin Champions", subtitle = "Europe's Top 5 League, 2017/18 - 2021/22") %>% 
  #set column header style
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style = list(
      cell_borders(sides = "bottom", weight = px(2)),
      cell_fill(color = textcol),
      cell_text(weight = "bold",
                color = bgcol),
      "vertical-align:middle"))


#xGApMA vs xGA graph ------
pma_by_team_season %>% 
  left_join(select(league_tables,c("team","season")), by = c("team","season")) %>%
  left_join(select(badge_set, c(team,badge)), by = "team") %>% 
  select(team, season, competition, Pos,norm_xGpMA_wt,norm_xGApMA_wt,badge) %>% 
  filter(Pos == 1) %>% 
  ggplot(aes(x = norm_xGpMA_wt, y = norm_xGApMA_wt, label = season, image = badge)) + 
  # geom_point(data = champions_xGpMA_wt, size = 7, shape = 1, show.legend = F, colour = "#ff0000") +  
  geom_image(size = 0.05) + 
  geom_label_repel(aes(colour = competition, fill = competition),
                  size = 4,
                  seed = 15,
                  force = 5,
                  family = plotfont,
                  show.legend = F)) +
  scale_fill_manual(values = competition_fills) +
  scale_colour_manual(values = competition_colours) +
  # scale_x_reverse()+
  # scale_alpha_manual(values = seq(1,0.5,-0.125), guide = "none") + 
  theme(plot.title = element_text(colour = textcol, family = titlefont, size = 40, face = "bold"),
        plot.subtitle = element_text(colour = textcol, family = "Roboto Slab Light", size = 15),
        legend.position = "top",
        legend.justification = "left",
        legend.background = element_rect(fill = bgcol, colour = bgcol),
        legend.key = element_rect(fill = bgcol, colour = bgcol),
        legend.title = element_blank(),
        legend.text = element_text(family = plotfont, colour = textcol, size = 20),
        plot.background = element_rect(fill = bgcol, colour = textcol),
        axis.text = element_text(family = plotfont, colour = textcol, size = 18),
        axis.title.y = element_text(family = plotfont, colour = textcol, size = 20),
        axis.title.x = element_text(family = plotfont, colour = textcol, size = 20),
        axis.line = element_line(colour = textcol),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = panelcol),
        plot.margin = margin(10,20,10,10),
        plot.caption = element_text(family = plotfont, colour = textcol, size = 18, hjust = 0)) +
  labs(title = "Narrow Margins",
       subtitle = paste(stringr::str_wrap("Weighted xG vs. Weighted xGA Minutes Spent in the Lead (or Minutes Ahead)
                       Europe's Top 5 Leagues, 2017/18 - 2021/22",75), collapse="\n"),
       caption = "1. Data courtesy of UnderStat\n2. Weighted xG Ahead = (xG accumulated while leading) x (1 + goal diff)/10 \n3. Weighted xGA Ahead = (xGA conceded while leading) x (1 - goal diff)/10",
       x = "Weighted xGpMA",
       y = "Weighted xGApMA")
