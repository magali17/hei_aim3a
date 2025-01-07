# Load the necessary package
pacman::p_load(DiagrammeR,
               DiagrammeRsvg, rsvg # convert DiagrammeR plot to SVG and then PNG
)

image_path <- file.path("..", "..", "Manuscript - Onroad data", "Images", "v5")

##############################################################################################################################

diagram <- '
digraph flowchart {
  rankdir=TB; // Top-to-Bottom orientation
  node [shape=rectangle, style=filled, fontcolor=black, fontname="Helvetica-bold", fontsize=16, margin=0.2];

  // Add annotations to the right of rows
  Annotation0 [label="Extensive Mobile\nMonitoring Campaign\n(On-Road & Roadside)", shape=none, fontsize=16, fillcolor="white", fontcolor="#E69F00"];
  Annotation1 [label="Reduced Sampling\nDecisions", shape=none, fontsize=16, fillcolor="white", fontcolor="#56B4E9"];
  Annotation2 [label="Analytic Decisions\nWith Collected Data", shape=none, fontsize=16, fillcolor="white", fontcolor="#0072B2"];
  Annotation3 [label="Epidemiological\nApplication", shape=none, fontsize=16, fillcolor="white", fontcolor="#009E73"];
  Annotation4 [label="Mobile Monitoring\nDesign Validation", shape=none, fontsize=16, fillcolor="white", fontcolor="#FFD700"];
  
  // Define nodes
  Start [label="On-Road UFP Data", shape=ellipse, fillcolor="#E69F00", width=1.5, height=0.75];
  VisitsPerLocation [label="Visits per Location", fillcolor="#56B4E9", width=1.5, height=0.75];
  SpatialBalance [label="Spatial Balance\nAcross Locations", fillcolor="#56B4E9", width=1.5, height=0.75];
  TimeSelection [label="Time Selection", fillcolor="#56B4E9", width=1.5, height=0.75];
  TemporalAdjustment [label="Temporal Adjustment", fillcolor="#0072B2", width=1.5, height=0.75];
  PlumeAdjustment [label="Plume Adjustment", fillcolor="#0072B2", width=1.5, height=0.75];
  ExposureModel [label="UFP Exposure Model", fillcolor="#009E73", width=1.5, height=0.75];
  HealthModel [label="Health Analysis:\nUFP & Cognitive Function", fillcolor="#009E73", width=1.5, height=0.75];
  Compare [label="Compare to Reference", shape=diamond, fillcolor="#FFD700", width=2.5, height=1];

  // Define edges
  Start -> VisitsPerLocation;
  VisitsPerLocation -> SpatialBalance;
  SpatialBalance -> TimeSelection;
  TimeSelection -> TemporalAdjustment;
  TemporalAdjustment -> PlumeAdjustment;
  PlumeAdjustment -> ExposureModel;
  ExposureModel -> HealthModel;
  ExposureModel -> Compare;
  HealthModel -> Compare;

  // Position annotations
  { rank=same; Annotation1; VisitsPerLocation; SpatialBalance; TimeSelection;  }
  { rank=same; Annotation2; TemporalAdjustment; PlumeAdjustment;  }
{ rank=same; Annotation3; ExposureModel; HealthModel; }
{ rank=same; Annotation4; Compare; }
}
'

# Generate and render the diagram
diagram <- grViz(diagram)

# Render the diagram
diagram


 

# Convert the diagram to SVG
diagram_svg <- export_svg(diagram)

# Save the SVG as a PNG
rsvg_png(charToRaw(diagram_svg), file = file.path(image_path,"abstract_art.png"),
         height = 1000, # pixels, not inches
         width = 1500
         )

