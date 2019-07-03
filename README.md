# BNLearnOR

This library intends to simplify producing Odds Ratios from BNLearn fit objects relatively straightforward.

## Usage

It requires a BNlearn fit object, and a configuration yaml file, and is used like so:

```r
config <- yaml::read_yaml('config.yml')
Produce.OR(fit, config)
```

This will create one, or multiple, forest plots dependent on the configuration file.

## Configuration File

The following is an example of the configuration file for the ALARM dataset:

```yaml
target_variable: 'BP'
target_levels: ['Low', 'High']
target_reference: 'Normal'

variable_levels:
  HIST: {'0':'False', 'True'}
  CVP: {'0':'Low', '0':'High', '0':'Normal', 'Reference':'Normal'}
 ```
 
 The variables specified in variable levels will appear in the plot.
 Here two plots will be created, one: Normal vs. Low, and two: Normal vs. High.
 
 Reference levels for each variable can be specified in the variable_levels, if none is provided, the first level specficied will be the reference.
