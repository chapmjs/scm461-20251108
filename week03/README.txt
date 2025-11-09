WEEK 3 ASSIGNMENT: Shortage Detection Dashboard

SCENARIO:
Stacey needs an interactive dashboard to identify supply shortages before 
they happen. Build a Shiny app that calculates supply (inventory + 
scheduled receipts + capacity) vs. demand for any part over a 6-month 
planning horizon.

YOUR TASKS:

STEP 1: Proof of Concept (proof_of_concept.R)
- Load data files
- Manually calculate supply/demand/shortage for one part (P-102)
- Verify your logic against expected_results.txt
- COMPLETE THIS BEFORE BUILDING SHINY APP

STEP 2: Build Shiny Dashboard (shortage_dashboard.R)
- UI: Dropdown to select part + button to trigger analysis
- Server: Calculate supply/demand/shortages for 6 months
- Outputs: Table, warnings, plot
- Test with all 5 parts

DELIVERABLE:
- proof_of_concept.R (validation script)
- shortage_dashboard.R (Shiny app)
- Brief README explaining how to run

DUE: [Date]

DATA FILES (in /data folder):
- parts_master.csv: Part numbers and descriptions
- current_inventory.csv: Current stock levels
- capacity_data.csv: Monthly production capacity
- scheduled_receipts.csv: Orders already placed
- demand_forecasts.csv: Expected demand

KEY CONCEPTS:
- Supply = Inventory + Scheduled Receipts + Capacity
- Shortage = Demand - Supply (when demand exceeds supply)
- Inventory carries forward month-to-month

EVALUATION ACTIVITY:
Before starting, review and run the three example apps in 
/evaluation_examples. We'll discuss:
- What makes an app functional vs. professional?
- Common errors in shortage calculations
- How to make dashboards user-friendly

VALIDATION:
Your app should produce results matching expected_results.txt

TIPS:
- Read Goal chapters 11-13 for context (bottleneck identification)
- Start with proof of concept to verify logic
- Test reactivity: Does clicking button update outputs?
- Use req() to prevent errors when inputs not ready
- Add print() statements in server to debug

SHINY BASICS REMINDER:
- UI: What user sees (inputs + outputs)
- Server: What happens behind scenes (calculations)
- Reactive: Code that automatically updates when inputs change
- eventReactive: Code that only runs when specific input changes (button)
