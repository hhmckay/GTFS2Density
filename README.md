# GTFS 2 Density
### Link: https://hhmckay.shinyapps.io/GTFSToDensity/
The GTFS2Density tool is intended to produce a simple transit route-level visualization of surrounding population and employment density, using a GTFS feed as the input. The tool works as follows:
1. Read a zipped GTFS feed and filter to a specified trip based on user selection.
2. Access population and employment statistics from the US Census and LEHD APIs.
3. Generate points along the GTFS shape at a specified interval (defined by the Distance Granularity input). The interval paramter is in meters. More granular intervals will produce a more detailed graph, but take longer to run.
4. Convert Census geometry-level population and employment data to points. Points are randomly-generated based on the Land Use Granularity parameter. This parameter controls how many of a given unit (i.e. people and jobs) a single point represents. Lower values will create a more detailed graph but  will take longer to run.
5. Create buffers around each route point and calculate the number of people/jobs within each buffer.
6. Organize data into an appropriate dataframe and adjust values to create a stacked area chart without double counting.

This application is very much a rough draft and is only set up to run analysis in CA.

**App GUI**
![AppGUI](https://github.com/user-attachments/assets/499ec07e-4e5c-4ca4-862f-de500537df85)

**Graph**
![SacRT51](https://github.com/user-attachments/assets/59df093e-9052-451b-9565-71c5367dc8df)
