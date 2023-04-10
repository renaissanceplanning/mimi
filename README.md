# mimi

`mimi` -- the **M**odifiable **I**nteractive **M**apping **I**nterface -- lays the groundwork for exploratory interactive mapping. It is designed as a product to get you "90% of the way there" for interactive mapping as part of internal project review. Gone are the days of spending hours building an application for reviewing your results; our friend Mimi is happy to do that work for you!

## Cool man! Nice! And creative name! But also, what's all this anyway?

`mimi` is not set up as a package, but rather as an example. The idea is that a user should be able to copy this application and use it "straight up", or make minimal project-specific changes to the generation of inputs and/or the server side of the application to get it up and running as desired (e.g. data formatting, color ramps). However, it is most easily employed by sticking to the default application, and ensuring that the input tables match the required `mimi` data structure. As such, the "example_application" folder contains, well, an example application in full. The `app.R` script is the application, which preps the inputs and runs the app. The "Data" subdirectory contains example data that conforms to the `mimi` data structure.

## Little different that what you normally do, but makes sense. So what's in the `app.R`?

The `app.R` defines the application, and ideally needs minimal work. It is divided into four sections for clear definition of tasks, and comments in the code should orient a user to their purpose, and what should/shouldn't be changed.

### Dependencies

Check out the "libraries" at the top of the app to see the dependencies. Most are CRAN, but you'll need a couple RPG packages here too.

### Globals

The "Globals" section defines a title and a basemap. The application title *is the only thing that absolutely needs to be changed* between project applications. The basemap *can* be changed, but usually shouldn't need to; it's the Renaissance default, and Brian did a sick job setting that up. Give the man some love!

### Data

The "Data" section loads and processes the inputs. If the data structure is followed (which is very desireable), then nothing here needs to be changed. But if you're making any on-the-fly changes to your data on read, this is where you do it.

### UI

This defines the map user interface. DON'T TOUCH IT. Please <3 Otherwise, you're likely in for a broken application. BUT, if you have feature requests, let me know!

### Server

This defines the map server side. You won't want to update any of the data processing -- as that will most DEFINITELY break the application -- but you may want to update some of the palettes or feature renderings. You can do that toward the bottom of this section in the code.

### Run

Not much to change here -- this is just one line that tells the app to go.

## Easy enough. Now what's this "data structure" you keep talking about?

The `mimi` app is designed to work with data formatted in a very specific way. And though this sounds daunting, there's really only a couple of requirements (and one isn't even a "hard" requirement). To start, any datasets with variables you want to be mapped or containing map elements must live in the "Data" subdirectory (and this subdirectory must be called "Data"). Then, for each file:

1. It must be saved as a .rds `sf` object, with CRS = EPSG:4326
2. It must have a unique ID as the **first column**

That's it! Not so bad right?

Meeting the first requirement will make your life easy, but it's necessarily a "must". The data section of the app, by default, uses `readRDS()` to load the data, and assigns the .rds file name to the dataset for intelligible reference in the UI dropdowns. But, you *could* change this if you wanted to (i.e., if your data is in a different format, or a different CRS) -- see the comments in the "Data" section of `app.R` for more information. But really, the easiest thing to do is just meet this requirement when you save the data for the application. Getting the data to EPSG:4326 is crucial though: Leaflet (which is doing the mapping under the hood) won't use any other CRS!

Meeting the second requirement is a **MUST**. There's no way around it, and the app will *certainly* not perform as expected if this is not the case.

You can check out the "Data" in this example if you need a guide. These datasets adhere to these requirements perfectly (I should hope so, I made them!).

## Alright, so now I know where to edit the app if I want, and more importantly, how to build my data for it. But how does the `mimi` app work?

The `mimi` application is designed as a simple Shiny sidebar layout. The left side of the application allows a user to select what data they'd like to see mapped and the visualization method to be used; the right side of the application will show an interactive map reflecting the user-specified inputs. Below, we'll give a quick tour of these inputs

### **Data controls** (Sidebar column 1; in blue box)

`mimi` maps variables according to the following formula:

$$
\frac{\text{Primary variable (Adjustment operation) (Secondary variable)}}{\text{(Normalization variable)}}
$$

The only required input is the "primary variable", which is the foundation of producing a mapped output. The additional elements in parentheses in the above formula are *optional*, and can be used to produce more complicated "composite variables" to be mapped from the input data (provided that the primary variable is numeric). Available adjustment operations currently include subtraction, addition, or multiplication.

In the application, you select these variables as follows:

  - **Primary variable source**: the table in which the primary variable can be found
  - **Primary variable**: will appear once **primary variable source** is set; the variable to be mapped, or numerator term 1 in a composite variable.
  - **Adjustment variable source** (optional): the table in which the adjustment variable can be found.
    - An example use case is looking at the population in some year: set the primary variable to population.
  - **Adjustment variable**: will appear if **adjustment variable source** is set; numerator term 2 in a composite variable.
  - **Adjustment operation**: will appear if **adjustment variable source** is set; the mathematical operation for combining the **primary** and **adjustment** variables. Defaults to subtraction (-).
    - An example use case is looking at the change in population between two years: set primary variable to population in y_2, set adjustment variable to population in y_1, and set the adjustment operation to "-".
  - **Normalization variable source** (optional): the table in which the normalization variable can be found.
  - **Normalization variable**: will appear if **normalization variable source** is set; the denominator of a composite variable
    - An example use case is looking at population density: set primary variable to population, set normalization variable to zone area.

### **Visualization controls** (Sidebar column 2, top; in green box)

Four visualization options exist in `mimi`, each with their own set of parameters:

  - **Natural breaks**: if selected, a "Number of bins" text box will appear; enter the desired number of bins (integer). This is the default, and is generally a good place to start if you don't know the distribution of your underlying data a priori.
  - **Quantile**: if selected, a "Number of bins" text box will appear; enter the desired number of bins (integer). The data will be split into quantiles of $\frac{1}{\text{Number of bins}}$.
  - **Manual bins**: if selected, an "Input manual bins" text box will appear; enter desired bin cut points *separated by commas*. It is generally best to use this visualization method *after* one of the others, so you have a better idea of the range of your data.
  - **Continuous**: if selected, "Continuous scale min" and "Continuous scale max" text boxes will appear; *optionally* enter desired min/max values for the scale. This is the only visualization method that does not require further specification!
  
By default, `mimi` will use Renaissance brand color palettes from the `rpgcolorsr` package (thanks to Brian again!). These can be changed on the server side of the application by a confident R user (for example, everyone reading this probably knows I love mapping everything in viridis!).
  
### **Click the button below to produce the map** (Sidebar column 2, bottom; in green box)

Once you've got all your mapping parameters set, click the "Update map" button to produce the map. Note that *every time you change parameters, you will need to click this button again to update the map**. Because there are so many input parameters, allowing the app to update itself on the fly would result in severe lags, and no one wants that!

## Great, seems simple enough. So what will my map look like?

Upon selecting "Update map", the map will populate on the left side of the application (a simple loading icon will indicate that the request is processing). The map legend will be titled with the expression of the mapped variable, for clarity. The map itself is interactive, and can be explored as detailed below:

  - Click and drag to pan the map
  - Scroll to zoom the map in our out (or used the "+/-" icon in the top-left of the map for fixed zoom in/out)
  - Hover over any feature to reveal relevant attributes. The feature will be highlighted, and a popup will present the feature ID, as well as, in order, the primary, secondary, normalization, and composite variable values (for as many as are part of the mapped output)
  - Hover over the layers control -- the hamburger stack in the top-left of the map -- to reveal a menu for turning layers on and off. Uncheck any boxes to turn that layer off. This is helpful if, for example, place/road labels are getting in the way of reading the map
  - Use the "Zoom to {ID field}" dropdown to the top left of the map to zoom to a particular feature. Upon selecting the feature's unique ID in this dropdown, the map will automatically zoom to that feature! You can also select "None" in this dropdown to return to the original zoom.
  
## Dang Aaron, that sure was a concise and comprehensive summary! But if I have any questions, what should I do?

Did someone mention 2010s icon and Canadian hero Carly Rae Jepsen? Because the answer to that question is \~call me maybe!\~ Me being Aaron, that is -- I'm happy to help out with troubleshooting. Also, any of the R developers at Renaissance -- Joel and Brian -- should be able to offer guidance in a pinch!


