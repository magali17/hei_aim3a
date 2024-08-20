# Load the necessary package
pacman::p_load(DiagrammeR,
               DiagrammeRsvg, rsvg # convert DiagrammeR plot to SVG and then PNG
)

image_path <- file.path("..", "Manuscript", "Images", "v5", "other", "road")

##############################################################################################################################
# Define the diagram using Graphviz dot notation
diagram <- '
digraph flowchart {
  rankdir=TB;
  node [shape=rectangle, style=filled, fontcolor=black, color=lightgray, fontname="Helvetica-bold", fontsize=14, margin=0.2];

  Start [label="On-road data\n(~6k 100m segments)", shape=ellipse, fillcolor="#E69F00", fontcolor="black", width=2.5, height=1];
  VisitsPerLocation [label="Visits per Location", shape=rect, style="rounded,filled", fillcolor="#56B4E9", width=2.5, height=1];
  Visits4 [label="4 visits", fillcolor="white", width=2.5, height=0.8];
  Visits12 [label="12 visits", fillcolor="white", width=2.5, height=0.8];
  SpatialBalance [label="Spatial Balance", shape=rect, style="rounded,filled", fillcolor="#56B4E9", width=2.5, height=1];
  Balanced [label="Balanced Sampling\n(Same Visits)", shape=rect, style="rounded,filled", fillcolor="#56B4E9", width=2.5, height=1];
  Unbalanced [label="Unbalanced Sampling\n(Lognormal distribution)", shape=rect, style="rounded,filled", fillcolor="#56B4E9", width=2.5, height=1];
  Routes [label="Routes", fillcolor="white", width=2.5, height=0.8];
  RoadSegments [label="Road segments", fillcolor="white", width=2.5, height=0.8];
  RandomClusters [label="Random Clusters", fillcolor="white", width=2.5, height=0.8];
  SensibleClusters [label="Sensible Clusters", fillcolor="white", width=2.5, height=0.8];
  UnsensibleClusters [label="Unsensible Clusters", fillcolor="white", width=2.5, height=0.8];
  RoadType [label="Road Type", fillcolor="white", width=2.5, height=0.8];
  TimeSelection [label="Time Selection", shape=rect, style="rounded,filled", fillcolor="#56B4E9", width=2.5, height=1];
  AllDaysHours [label="All Days & Hours", fillcolor="white", width=2.5, height=0.8];
  WeekdayBusinessHours [label="Weekday Business Hours", fillcolor="white", width=2.5, height=0.8];
  TemporalAdjustment [label="Temporal Adjustment", shape=rect, style="rounded,filled", fillcolor="#0072B2", width=2.5, height=1];
  NoneTempAdj [label="None", fillcolor="white", width=2.5, height=0.8];
  FixedSiteAdj [label="Fixed Site Adjustment", fillcolor="white", width=2.5, height=0.8];
  UnderwriteAdj [label="Underwrite Adjustment", fillcolor="white", width=2.5, height=0.8];
  PlumeAdjustment [label="Plume Adjustment", shape=rect, style="rounded,filled", fillcolor="#0072B2", width=2.5, height=1];
  NonePlumeAdj [label="None", fillcolor="white", width=2.5, height=0.8];
  PlumeAdj [label="Plume Adjustment", fillcolor="white", width=2.5, height=0.8];
  AvgConcentrations [label="Annual Average Segment Concentrations", shape=rect, style="rounded,filled", fillcolor="#009E73", width=2.5, height=1];
  ExposureModel [label="On-Road Exposure Model", shape=rect, style="rounded,filled", fillcolor="#009E73", width=2.5, height=1];
  HealthModel [label="Health Model", shape=rect, style="rounded,filled", fillcolor="#009E73", width=2.5, height=1];
  Compare [label="Compare to roadside stationary model performance", shape=diamond, style="filled", fillcolor="#FFD700", width=3, height=1];
  CompareInference [label="Compare to roadside stationary model inference", shape=diamond, style="filled", fillcolor="#FFD700", width=3, height=1];

  # Define edges
  Start -> VisitsPerLocation [label="", color="black"];
  VisitsPerLocation -> Visits4 [label="", color="black"];
  VisitsPerLocation -> Visits12 [label="", color="black"];
  Visits4 -> SpatialBalance [label="", color="black"];
  Visits12 -> SpatialBalance [label="", color="black"];
  SpatialBalance -> Balanced [label="", color="black"];
  SpatialBalance -> Unbalanced [label="", color="black"];
  Balanced -> Routes [label="", color="black"];
  Balanced -> RoadSegments [label="", color="black"];
  Unbalanced -> RandomClusters [label="", color="black"];
  Unbalanced -> SensibleClusters [label="", color="black"];
  Unbalanced -> UnsensibleClusters [label="", color="black"];
  Unbalanced -> RoadType [label="", color="black"];
  Unbalanced -> RoadSegments [label="", color="black"];
  Routes -> TimeSelection [label="", color="black"];
  RoadSegments -> TimeSelection [label="", color="black"];
  RandomClusters -> TimeSelection [label="", color="black"];
  SensibleClusters -> TimeSelection [label="", color="black"];
  UnsensibleClusters -> TimeSelection [label="", color="black"];
  RoadType -> TimeSelection [label="", color="black"];
  TimeSelection -> AllDaysHours [label="", color="black"];
  TimeSelection -> WeekdayBusinessHours [label="", color="black"];
  AllDaysHours -> NoneTempAdj [label="", color="black"];
  WeekdayBusinessHours -> TemporalAdjustment [label="", color="black"];
  TemporalAdjustment -> NoneTempAdj [label="", color="black"];
  TemporalAdjustment -> FixedSiteAdj [label="", color="black"];
  TemporalAdjustment -> UnderwriteAdj [label="", color="black"];
  NoneTempAdj -> PlumeAdjustment [label="", color="black"];
  FixedSiteAdj -> PlumeAdjustment [label="", color="black"];
  UnderwriteAdj -> PlumeAdjustment [label="", color="black"];
  PlumeAdjustment -> NonePlumeAdj [label="", color="black"];
  PlumeAdjustment -> PlumeAdj [label="", color="black"];
  NonePlumeAdj -> AvgConcentrations [label="", color="black"];
  PlumeAdj -> AvgConcentrations [label="", color="black"];
  AvgConcentrations -> ExposureModel [label="", color="black"];
  ExposureModel -> HealthModel [label="", color="black"];
  ExposureModel -> Compare [label="", color="black"];
  HealthModel -> CompareInference [label="", color="black"];
  
  # Legend
  subgraph cluster_legend {
    label=<<B>Legend</B>>;
    fontsize=14;
    style=rounded;
    color=lightgray;
    node [shape=rect, style="filled", width=2.5, height=1, fontsize=14];
    
    key1 [label="Input", shape=ellipse, fillcolor="#E69F00"];
    key2 [label="Monitoring Decision", shape=rect, style="rounded,filled", fillcolor="#56B4E9"];
    key3 [label="Analytic Decision", shape=rect, style="rounded,filled", fillcolor="#0072B2"];
    key4 [label="Option", fillcolor="white"];
    key5 [label="Outcome", shape=rect, style="rounded,filled", fillcolor="#009E73"];
    key6 [label="Result", shape=diamond, fillcolor="#FFD700"];
    
    key1 -> key2 [style=invis];
    key2 -> key3 [style=invis];
    key3 -> key4 [style=invis];
    key4 -> key5 [style=invis];
    key5 -> key6 [style=invis];
    
    { rank=same; key1; key4; }
    { rank=same; key2; key5; }
    { rank=same; key3; key6; }
  }
}
'

# Generate the diagram
diagram <- grViz(diagram)

diagram

# Convert the diagram to SVG
diagram_svg <- export_svg(diagram)

# Save the SVG as a PNG
rsvg_png(charToRaw(diagram_svg), file = file.path(image_path,"diagram.png"))


##############################################################################################################################
# HEI REPORT
##############################################################################################################################
library(DiagrammeR)

hei_diagram <- '
digraph flowchart {
  rankdir=TB;
  node [shape=rectangle, style=filled, fontcolor=black, color=lightgray, fontname="Helvetica-bold", fontsize=14, margin=0.2];

  Start [label="On-road data\n(>5k 100m segments)", shape=ellipse, fillcolor="#E69F00", fontcolor="black", width=2.5, height=1];
  VisitsPerLocation [label="Visits per Location", shape=rect, style="rounded,filled", fillcolor="#56B4E9", width=2.5, height=1];
  Visits4 [label="4 visits", fillcolor="white", width=2.5, height=0.8];
  Visits12 [label="12 visits", fillcolor="white", width=2.5, height=0.8];
  SpatialBalance [label="Spatial Balance", shape=rect, style="rounded,filled", fillcolor="#56B4E9", width=2.5, height=1];
  Balanced [label="Balanced Sampling\n(Same Visits)", shape=rect, style="rounded,filled", fillcolor="#56B4E9", width=2.5, height=1];
  Unbalanced [label="Unbalanced Sampling\n(Lognormal distribution)", shape=rect, style="rounded,filled", fillcolor="#56B4E9", width=2.5, height=1];
  RoadSegments [label="Road segments", fillcolor="white", width=2.5, height=0.8];
  RandomClusters [label="Random Clusters", fillcolor="white", width=2.5, height=0.8];
  SensibleClusters [label="Sensible Clusters", fillcolor="white", width=2.5, height=0.8];
  UnsensibleClusters [label="Unsensible Clusters", fillcolor="white", width=2.5, height=0.8];
  RoadType [label="Road Type", fillcolor="white", width=2.5, height=0.8];
  TimeSelection [label="Time Selection", shape=rect, style="rounded,filled", fillcolor="#56B4E9", width=2.5, height=1];
  AllDaysHours [label="All Days & Hours", fillcolor="white", width=2.5, height=0.8];
  WeekdayBusinessHours [label="Weekday Business Hours", fillcolor="white", width=2.5, height=0.8];
  TemporalAdjustment [label="Temporal Adjustment", shape=rect, style="rounded,filled", fillcolor="#0072B2", width=2.5, height=1];
  NoneTempAdj [label="None", fillcolor="white", width=2.5, height=0.8];
  FixedSiteAdj [label="Fixed Site Adjustment", fillcolor="white", width=2.5, height=0.8];
  PlumeAdjustment [label="Plume Adjustment", shape=rect, style="rounded,filled", fillcolor="#0072B2", width=2.5, height=1];
  NonePlumeAdj [label="None", fillcolor="white", width=2.5, height=0.8];
  PlumeAdj [label="Plume Adjustment", fillcolor="white", width=2.5, height=0.8];
  AvgConcentrations [label="Annual Average Segment Concentrations", shape=rect, style="rounded,filled", fillcolor="#009E73", width=2.5, height=1];
  ExposureModel [label="On-Road Exposure Model", shape=rect, style="rounded,filled", fillcolor="#009E73", width=2.5, height=1];
  HealthModel [label="Health Model", shape=rect, style="rounded,filled", fillcolor="#009E73", width=2.5, height=1];
  Compare [label="Compare to roadside stationary model performance", shape=diamond, style="filled", fillcolor="#FFD700", width=3, height=1];
  CompareInference [label="Compare to roadside stationary model inference", shape=diamond, style="filled", fillcolor="#FFD700", width=3, height=1];

  # Define edges
  Start -> VisitsPerLocation [label="", color="black"];
  VisitsPerLocation -> Visits4 [label="", color="black"];
  VisitsPerLocation -> Visits12 [label="", color="black"];
  Visits4 -> SpatialBalance [label="", color="black"];
  Visits12 -> SpatialBalance [label="", color="black"];
  SpatialBalance -> Balanced [label="", color="black"];
  SpatialBalance -> Unbalanced [label="", color="black"];
  Balanced -> RoadSegments [label="", color="black"];
  Unbalanced -> RandomClusters [label="", color="black"];
  Unbalanced -> SensibleClusters [label="", color="black"];
  Unbalanced -> UnsensibleClusters [label="", color="black"];
  Unbalanced -> RoadType [label="", color="black"];
  Unbalanced -> RoadSegments [label="", color="black"];
  RoadSegments -> TimeSelection [label="", color="black"];
  RandomClusters -> TimeSelection [label="", color="black"];
  SensibleClusters -> TimeSelection [label="", color="black"];
  UnsensibleClusters -> TimeSelection [label="", color="black"];
  RoadType -> TimeSelection [label="", color="black"];
  TimeSelection -> AllDaysHours [label="", color="black"];
  TimeSelection -> WeekdayBusinessHours [label="", color="black"];
  AllDaysHours -> NoneTempAdj [label="", color="black"];
  WeekdayBusinessHours -> TemporalAdjustment [label="", color="black"];
  TemporalAdjustment -> NoneTempAdj [label="", color="black"];
  TemporalAdjustment -> FixedSiteAdj [label="", color="black"];
  NoneTempAdj -> PlumeAdjustment [label="", color="black"];
  FixedSiteAdj -> PlumeAdjustment [label="", color="black"];
  PlumeAdjustment -> NonePlumeAdj [label="", color="black"];
  PlumeAdjustment -> PlumeAdj [label="", color="black"];
  NonePlumeAdj -> AvgConcentrations [label="", color="black"];
  PlumeAdj -> AvgConcentrations [label="", color="black"];
  AvgConcentrations -> ExposureModel [label="", color="black"];
  ExposureModel -> HealthModel [label="", color="black"];
  ExposureModel -> Compare [label="", color="black"];
  HealthModel -> CompareInference [label="", color="black"];
  
  # Legend
  subgraph cluster_legend {
    label=<<B>Legend</B>>;
    fontsize=14;
    style=rounded;
    color=lightgray;
    node [shape=rect, style="filled", width=2.5, height=1, fontsize=14];
    
    key1 [label="Input", shape=ellipse, fillcolor="#E69F00"];
    key2 [label="Monitoring Decision", shape=rect, style="rounded,filled", fillcolor="#56B4E9"];
    key3 [label="Analytic Decision", shape=rect, style="rounded,filled", fillcolor="#0072B2"];
    key4 [label="Option", fillcolor="white"];
    key5 [label="Outcome", shape=rect, style="rounded,filled", fillcolor="#009E73"];
    key6 [label="Result", shape=diamond, fillcolor="#FFD700"];
    
    key1 -> key2 [style=invis];
    key2 -> key3 [style=invis];
    key3 -> key4 [style=invis];
    key4 -> key5 [style=invis];
    key5 -> key6 [style=invis];
    
    { rank=same; key1; key4; }
    { rank=same; key2; key5; }
    { rank=same; key3; key6; }
  }
}
'

# Generate the diagram
hei_diagram <- grViz(hei_diagram)

hei_diagram

# Convert the diagram to SVG
hei_diagram_svg <- export_svg(hei_diagram)

# Save the SVG as a PNG
rsvg_png(charToRaw(hei_diagram_svg), file = file.path(image_path, "HEI Report", "hei_diagram.png"))
