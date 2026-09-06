# Count-cumulative full-sweep state results

Finite settlement target: `C_t(26)`. Intervals are 90% intervals.

The interval tables were produced with `tbl.now::tidy()` from 250
posterior-predictive draws per fit. WIS and coverage are diagnostic
composite-likelihood pseudo-posterior scores, not calibration proof.

## Stability summary



|clock      |model              | fits| finite| optimizer_converged| gradient_below_01| leakage_passed| laplace_fits| median_gradient| max_gradient|
|:----------|:------------------|----:|------:|-------------------:|-----------------:|--------------:|------------:|---------------:|------------:|
|calendar   |cumulative_nb      |  265|    265|                 265|               265|            265|          265|          0.0012|       0.0213|
|calendar   |cumulative_poisson |  265|    265|                 265|               265|            265|          265|          0.0022|       0.0497|
|calendar   |hurdle_ztnb        |  265|    265|                 265|               265|            265|            0|          0.0005|       0.0091|
|calendar   |hurdle_ztpoisson   |  265|    265|                 260|               260|            265|            0|          0.0097|       9.6765|
|compressed |cumulative_nb      |  265|    265|                 265|               265|            265|          265|          0.0012|       0.0273|
|compressed |cumulative_poisson |  265|    265|                 265|               265|            265|          265|          0.0011|       0.0253|
|compressed |hurdle_ztnb        |  265|    265|                 265|               265|            265|            0|          0.0005|       0.0144|
|compressed |hurdle_ztpoisson   |  265|    265|                 265|               265|            265|            0|          0.0055|       0.0632|

## Aggregate score summary



|clock      |model                |    n|     wis|     mae| coverage90|
|:----------|:--------------------|----:|-------:|-------:|----------:|
|calendar   |cumulative_nb        | 1325| 67.9477| 95.6377|     0.6362|
|calendar   |cumulative_poisson   | 1325| 32.2393| 34.9958|     0.6181|
|calendar   |empirical_multiplier | 1325| 38.2174| 38.2174|     0.3932|
|calendar   |hurdle_ztnb          | 1325| 30.7148| 36.8226|     0.7970|
|calendar   |hurdle_ztpoisson     | 1325| 36.9467| 39.4215|     0.6506|
|compressed |cumulative_nb        | 1325| 31.9910| 34.4332|     0.6143|
|compressed |cumulative_poisson   | 1325| 30.7247| 32.8475|     0.5917|
|compressed |empirical_multiplier | 1325| 38.3419| 38.3419|     0.4075|
|compressed |hurdle_ztnb          | 1325| 31.9004| 36.9509|     0.7577|
|compressed |hurdle_ztpoisson     | 1325| 39.8889| 47.7928|     0.6604|

## Per-state metrics



|location             |clock      |model                | targets|  mean_wis| mean_absolute_error| median_absolute_error| median_signed_error| coverage90|
|:--------------------|:----------|:--------------------|-------:|---------:|-------------------:|---------------------:|-------------------:|----------:|
|Alabama              |calendar   |cumulative_nb        |      25|    3.2195|                4.60|                   1.0|                 0.0|       0.92|
|Alabama              |calendar   |cumulative_poisson   |      25|    4.7170|                6.20|                   1.0|                 0.0|       0.68|
|Alabama              |calendar   |empirical_multiplier |      25|    1.9600|                1.96|                   0.0|                 0.0|       0.72|
|Alabama              |calendar   |hurdle_ztnb          |      25|    1.5779|                2.00|                   0.0|                 0.0|       0.96|
|Alabama              |calendar   |hurdle_ztpoisson     |      25|    1.6993|                2.08|                   0.0|                 0.0|       0.80|
|Alabama              |compressed |cumulative_nb        |      25|    2.5102|                3.50|                   1.0|                -1.0|       0.80|
|Alabama              |compressed |cumulative_poisson   |      25|    2.4993|                3.38|                   1.0|                -1.0|       0.80|
|Alabama              |compressed |empirical_multiplier |      25|    1.9600|                1.96|                   0.0|                 0.0|       0.72|
|Alabama              |compressed |hurdle_ztnb          |      25|    2.1656|                2.20|                   0.0|                 0.0|       0.84|
|Alabama              |compressed |hurdle_ztpoisson     |      25|    4.2367|                6.12|                   3.0|                -3.0|       0.88|
|Alaska               |calendar   |cumulative_nb        |      25|    1.9358|                2.80|                   2.0|                -2.0|       0.72|
|Alaska               |calendar   |cumulative_poisson   |      25|    1.9525|                2.72|                   2.0|                -2.0|       0.72|
|Alaska               |calendar   |empirical_multiplier |      25|    3.6800|                3.68|                   2.0|                -2.0|       0.04|
|Alaska               |calendar   |hurdle_ztnb          |      25|    2.4762|                3.52|                   2.0|                -2.0|       0.80|
|Alaska               |calendar   |hurdle_ztpoisson     |      25|    2.2445|                3.42|                   2.0|                -2.0|       0.84|
|Alaska               |compressed |cumulative_nb        |      25|    2.9920|                3.70|                   2.0|                -2.0|       0.44|
|Alaska               |compressed |cumulative_poisson   |      25|    3.0332|                3.72|                   2.0|                -2.0|       0.44|
|Alaska               |compressed |empirical_multiplier |      25|    3.8400|                3.84|                   2.0|                -2.0|       0.08|
|Alaska               |compressed |hurdle_ztnb          |      25|    3.1828|                3.68|                   2.0|                -2.0|       0.40|
|Alaska               |compressed |hurdle_ztpoisson     |      25|    3.0566|                3.64|                   2.0|                -2.0|       0.32|
|Arizona              |calendar   |cumulative_nb        |      25|  153.5936|              199.88|                 138.5|               138.5|       0.12|
|Arizona              |calendar   |cumulative_poisson   |      25|   35.0853|               39.60|                  22.0|                -3.0|       0.24|
|Arizona              |calendar   |empirical_multiplier |      25|   19.2400|               19.24|                  17.0|                -9.0|       0.04|
|Arizona              |calendar   |hurdle_ztnb          |      25|   33.4157|               21.42|                  22.0|                -3.5|       0.92|
|Arizona              |calendar   |hurdle_ztpoisson     |      25|   26.0385|               37.92|                  29.0|               -27.0|       0.72|
|Arizona              |compressed |cumulative_nb        |      25|   17.1117|               18.14|                  16.0|                -7.0|       0.36|
|Arizona              |compressed |cumulative_poisson   |      25|   17.8463|               19.88|                  16.0|                -6.0|       0.24|
|Arizona              |compressed |empirical_multiplier |      25|   18.9200|               18.92|                  16.0|                -9.0|       0.04|
|Arizona              |compressed |hurdle_ztnb          |      25|   26.6477|               19.30|                  16.0|                -9.0|       0.92|
|Arizona              |compressed |hurdle_ztpoisson     |      25|   36.9028|               54.56|                  31.0|               -29.0|       0.68|
|Arkansas             |calendar   |cumulative_nb        |      25|    8.4721|               10.04|                   2.0|                -1.0|       0.64|
|Arkansas             |calendar   |cumulative_poisson   |      25|    9.8780|               11.10|                   1.0|                -1.0|       0.64|
|Arkansas             |calendar   |empirical_multiplier |      25|   14.7600|               14.76|                   2.0|                -2.0|       0.36|
|Arkansas             |calendar   |hurdle_ztnb          |      25|   12.2435|               14.30|                   2.0|                -2.0|       0.80|
|Arkansas             |calendar   |hurdle_ztpoisson     |      25|   10.9213|               13.60|                   3.0|                -1.0|       0.68|
|Arkansas             |compressed |cumulative_nb        |      25|   11.7386|               13.18|                   1.0|                -1.0|       0.68|
|Arkansas             |compressed |cumulative_poisson   |      25|   12.0879|               13.48|                   2.0|                -2.0|       0.68|
|Arkansas             |compressed |empirical_multiplier |      25|   14.0400|               14.04|                   1.0|                -1.0|       0.40|
|Arkansas             |compressed |hurdle_ztnb          |      25|   11.6879|               13.34|                   1.0|                -1.0|       0.84|
|Arkansas             |compressed |hurdle_ztpoisson     |      25|    9.8575|               12.40|                   1.0|                -1.0|       0.72|
|California           |calendar   |cumulative_nb        |      25|  232.5051|              306.06|                 210.0|               210.0|       0.08|
|California           |calendar   |cumulative_poisson   |      25|   93.5657|              100.34|                  24.0|                -8.0|       0.28|
|California           |calendar   |empirical_multiplier |      25|  109.8400|              109.84|                  24.0|               -24.0|       0.00|
|California           |calendar   |hurdle_ztnb          |      25|   89.8969|              104.46|                  21.5|               -21.5|       0.64|
|California           |calendar   |hurdle_ztpoisson     |      25|  105.7386|              109.84|                  24.0|               -24.0|       0.04|
|California           |compressed |cumulative_nb        |      25|   78.9376|               84.14|                  11.0|               -11.0|       0.40|
|California           |compressed |cumulative_poisson   |      25|   78.3276|               82.98|                  11.0|               -11.0|       0.40|
|California           |compressed |empirical_multiplier |      25|  105.0400|              105.04|                  24.0|               -24.0|       0.00|
|California           |compressed |hurdle_ztnb          |      25|   90.0631|               98.72|                  16.5|               -16.5|       0.68|
|California           |compressed |hurdle_ztpoisson     |      25|   85.2830|               97.58|                  38.5|               -38.5|       0.28|
|Colorado             |calendar   |cumulative_nb        |      25|    4.8396|                5.92|                   2.0|                 0.0|       0.88|
|Colorado             |calendar   |cumulative_poisson   |      25|    7.0559|                8.68|                   2.0|                 0.0|       0.64|
|Colorado             |calendar   |empirical_multiplier |      25|    2.0000|                2.00|                   0.0|                 0.0|       0.60|
|Colorado             |calendar   |hurdle_ztnb          |      25|    1.7317|                1.82|                   0.5|                 0.0|       0.96|
|Colorado             |calendar   |hurdle_ztpoisson     |      25|    1.7564|                2.28|                   0.0|                 0.0|       0.80|
|Colorado             |compressed |cumulative_nb        |      25|    1.6754|                2.10|                   1.0|                 0.0|       0.88|
|Colorado             |compressed |cumulative_poisson   |      25|    1.6739|                2.02|                   1.0|                 0.0|       0.88|
|Colorado             |compressed |empirical_multiplier |      25|    2.0000|                2.00|                   0.0|                 0.0|       0.60|
|Colorado             |compressed |hurdle_ztnb          |      25|    1.5444|                1.84|                   0.0|                 0.0|       0.92|
|Colorado             |compressed |hurdle_ztpoisson     |      25|    1.5644|                1.80|                   0.0|                 0.0|       0.84|
|Connecticut          |calendar   |cumulative_nb        |      25|    3.7322|                4.76|                   1.0|                 0.0|       0.84|
|Connecticut          |calendar   |cumulative_poisson   |      25|    4.8637|                5.56|                   1.0|                 0.0|       0.68|
|Connecticut          |calendar   |empirical_multiplier |      25|    2.4000|                2.40|                   0.0|                 0.0|       0.64|
|Connecticut          |calendar   |hurdle_ztnb          |      25|    2.3301|                2.52|                   0.0|                 0.0|       0.96|
|Connecticut          |calendar   |hurdle_ztpoisson     |      25|    1.9869|                2.36|                   0.0|                 0.0|       0.88|
|Connecticut          |compressed |cumulative_nb        |      25|    3.2439|                3.24|                   0.0|                 0.0|       0.72|
|Connecticut          |compressed |cumulative_poisson   |      25|    3.2945|                3.10|                   0.0|                 0.0|       0.72|
|Connecticut          |compressed |empirical_multiplier |      25|    2.6000|                2.60|                   0.0|                 0.0|       0.64|
|Connecticut          |compressed |hurdle_ztnb          |      25|    3.0505|                2.60|                   0.0|                 0.0|       0.76|
|Connecticut          |compressed |hurdle_ztpoisson     |      25|    3.9092|                4.74|                   0.0|                 0.0|       0.68|
|Delaware             |calendar   |cumulative_nb        |      25|   15.7657|               17.02|                   9.0|                -9.0|       0.36|
|Delaware             |calendar   |cumulative_poisson   |      25|   15.1237|               16.40|                   9.0|                -9.0|       0.44|
|Delaware             |calendar   |empirical_multiplier |      25|   17.7200|               17.72|                   9.0|                -9.0|       0.24|
|Delaware             |calendar   |hurdle_ztnb          |      25|   16.8987|               17.72|                   9.0|                -9.0|       0.44|
|Delaware             |calendar   |hurdle_ztpoisson     |      25|   15.1755|               17.56|                   9.0|                -9.0|       0.48|
|Delaware             |compressed |cumulative_nb        |      25|   16.7957|               17.80|                   9.0|                -9.0|       0.40|
|Delaware             |compressed |cumulative_poisson   |      25|   16.8444|               17.90|                   9.0|                -9.0|       0.36|
|Delaware             |compressed |empirical_multiplier |      25|   17.9600|               17.96|                   9.0|                -9.0|       0.24|
|Delaware             |compressed |hurdle_ztnb          |      25|   17.4962|               17.96|                   9.0|                -9.0|       0.40|
|Delaware             |compressed |hurdle_ztpoisson     |      25|   16.3740|               17.96|                   9.0|                -9.0|       0.36|
|District of Columbia |calendar   |cumulative_nb        |      25|    2.8570|                3.40|                   0.5|                 0.0|       0.68|
|District of Columbia |calendar   |cumulative_poisson   |      25|    2.8560|                3.40|                   1.0|                 0.0|       0.72|
|District of Columbia |calendar   |empirical_multiplier |      25|    3.8400|                3.84|                   1.0|                 0.0|       0.48|
|District of Columbia |calendar   |hurdle_ztnb          |      25|    3.4590|                3.86|                   1.0|                 0.0|       0.60|
|District of Columbia |calendar   |hurdle_ztpoisson     |      25|    2.9555|                3.70|                   2.0|                 0.0|       0.68|
|District of Columbia |compressed |cumulative_nb        |      25|    2.9855|                3.84|                   1.0|                 0.0|       0.68|
|District of Columbia |compressed |cumulative_poisson   |      25|    3.0996|                3.88|                   1.0|                 0.0|       0.68|
|District of Columbia |compressed |empirical_multiplier |      25|    3.8400|                3.84|                   1.0|                 0.0|       0.48|
|District of Columbia |compressed |hurdle_ztnb          |      25|    3.3714|                3.84|                   1.0|                 0.0|       0.56|
|District of Columbia |compressed |hurdle_ztpoisson     |      25|    2.7209|                3.62|                   1.0|                 0.0|       0.76|
|Florida              |calendar   |cumulative_nb        |      25|  244.2415|              325.50|                 225.0|               225.0|       0.00|
|Florida              |calendar   |cumulative_poisson   |      25|   46.4583|               52.96|                  31.0|                -4.0|       0.32|
|Florida              |calendar   |empirical_multiplier |      25|   66.4800|               66.48|                  28.0|               -28.0|       0.00|
|Florida              |calendar   |hurdle_ztnb          |      25|   44.6011|               60.34|                  22.5|               -22.5|       0.80|
|Florida              |calendar   |hurdle_ztpoisson     |      25|   62.6681|               66.48|                  28.0|               -28.0|       0.12|
|Florida              |compressed |cumulative_nb        |      25|   53.9653|               60.20|                  22.0|               -22.0|       0.20|
|Florida              |compressed |cumulative_poisson   |      25|   52.4292|               58.40|                  22.0|               -22.0|       0.24|
|Florida              |compressed |empirical_multiplier |      25|   66.0400|               66.04|                  28.0|               -28.0|       0.00|
|Florida              |compressed |hurdle_ztnb          |      25|   54.9416|               61.54|                  25.0|               -25.0|       0.56|
|Florida              |compressed |hurdle_ztpoisson     |      25|   48.0730|               56.48|                  21.5|               -21.5|       0.48|
|Georgia              |calendar   |cumulative_nb        |      25|   10.7532|               12.60|                   2.0|                 1.0|       0.84|
|Georgia              |calendar   |cumulative_poisson   |      25|   14.6930|               17.06|                   2.0|                -1.0|       0.64|
|Georgia              |calendar   |empirical_multiplier |      25|   25.2400|               25.24|                   2.0|                -2.0|       0.12|
|Georgia              |calendar   |hurdle_ztnb          |      25|   21.3970|               24.26|                   4.0|                -1.0|       0.84|
|Georgia              |calendar   |hurdle_ztpoisson     |      25|   20.3207|               23.76|                   3.0|                -2.0|       0.80|
|Georgia              |compressed |cumulative_nb        |      25|   15.6906|               17.54|                   2.0|                -1.0|       0.80|
|Georgia              |compressed |cumulative_poisson   |      25|   15.0526|               16.84|                   2.0|                -1.0|       0.80|
|Georgia              |compressed |empirical_multiplier |      25|   24.1200|               24.12|                   2.0|                -2.0|       0.12|
|Georgia              |compressed |hurdle_ztnb          |      25|   20.5210|               23.98|                   4.0|                -0.5|       0.84|
|Georgia              |compressed |hurdle_ztpoisson     |      25|   14.9296|               17.22|                   1.5|                -1.0|       0.96|
|Hawaii               |calendar   |cumulative_nb        |      25|    2.0889|                3.22|                   1.0|                 1.0|       0.96|
|Hawaii               |calendar   |cumulative_poisson   |      25|    2.2840|                3.44|                   2.0|                 2.0|       0.96|
|Hawaii               |calendar   |empirical_multiplier |      25|    0.6000|                0.60|                   0.0|                 0.0|       0.76|
|Hawaii               |calendar   |hurdle_ztnb          |      25|    0.7455|                0.64|                   0.0|                 0.0|       0.96|
|Hawaii               |calendar   |hurdle_ztpoisson     |      25|    1.1716|                1.08|                   0.0|                 0.0|       1.00|
|Hawaii               |compressed |cumulative_nb        |      25|    1.3629|                1.50|                   0.0|                 0.0|       0.92|
|Hawaii               |compressed |cumulative_poisson   |      25|    1.3618|                1.44|                   0.0|                 0.0|       0.92|
|Hawaii               |compressed |empirical_multiplier |      25|    1.2000|                1.20|                   0.0|                 0.0|       0.72|
|Hawaii               |compressed |hurdle_ztnb          |      25|    1.1994|                1.24|                   0.0|                 0.0|       0.92|
|Hawaii               |compressed |hurdle_ztpoisson     |      25|    1.3439|                1.60|                   0.0|                 0.0|       0.92|
|Idaho                |calendar   |cumulative_nb        |      25|    2.0283|                2.80|                   1.0|                 0.0|       0.88|
|Idaho                |calendar   |cumulative_poisson   |      25|    2.0988|                2.90|                   1.0|                 0.0|       0.80|
|Idaho                |calendar   |empirical_multiplier |      25|    3.2400|                3.24|                   0.0|                 0.0|       0.56|
|Idaho                |calendar   |hurdle_ztnb          |      25|    2.6081|                3.24|                   0.0|                 0.0|       0.80|
|Idaho                |calendar   |hurdle_ztpoisson     |      25|    2.6000|                3.24|                   0.0|                 0.0|       0.84|
|Idaho                |compressed |cumulative_nb        |      25|    2.4868|                3.28|                   1.0|                 0.0|       0.76|
|Idaho                |compressed |cumulative_poisson   |      25|    2.6976|                3.24|                   1.0|                 0.0|       0.76|
|Idaho                |compressed |empirical_multiplier |      25|    3.2800|                3.28|                   0.0|                 0.0|       0.60|
|Idaho                |compressed |hurdle_ztnb          |      25|    2.9851|                3.32|                   0.0|                 0.0|       0.72|
|Idaho                |compressed |hurdle_ztpoisson     |      25|    2.8245|                3.30|                   0.0|                 0.0|       0.68|
|Illinois             |calendar   |cumulative_nb        |      25|   12.8516|               17.54|                   5.0|                 1.0|       0.72|
|Illinois             |calendar   |cumulative_poisson   |      25|   18.9745|               21.96|                   5.0|                -1.0|       0.60|
|Illinois             |calendar   |empirical_multiplier |      25|   31.3200|               31.32|                   4.0|                -4.0|       0.16|
|Illinois             |calendar   |hurdle_ztnb          |      25|   26.6200|               29.98|                   5.0|                -2.0|       0.72|
|Illinois             |calendar   |hurdle_ztpoisson     |      25|   29.0044|               30.44|                   4.5|                -3.0|       0.32|
|Illinois             |compressed |cumulative_nb        |      25|   21.4776|               26.00|                   3.0|                -3.0|       0.48|
|Illinois             |compressed |cumulative_poisson   |      25|   21.2727|               25.04|                   3.0|                -1.0|       0.52|
|Illinois             |compressed |empirical_multiplier |      25|   29.8400|               29.84|                   4.0|                -4.0|       0.20|
|Illinois             |compressed |hurdle_ztnb          |      25|   26.5105|               28.84|                   6.0|                -1.0|       0.72|
|Illinois             |compressed |hurdle_ztpoisson     |      25|   26.6151|               30.34|                   6.5|                -6.5|       0.52|
|Indiana              |calendar   |cumulative_nb        |      25|   23.6110|               27.22|                   8.0|                -4.0|       0.56|
|Indiana              |calendar   |cumulative_poisson   |      25|   29.1606|               33.04|                  10.5|                -6.0|       0.48|
|Indiana              |calendar   |empirical_multiplier |      25|   19.7600|               19.76|                   1.0|                -1.0|       0.48|
|Indiana              |calendar   |hurdle_ztnb          |      25|   18.1596|               19.84|                   1.0|                 0.0|       0.80|
|Indiana              |calendar   |hurdle_ztpoisson     |      25|   26.6317|               34.64|                   1.0|                -1.0|       0.68|
|Indiana              |compressed |cumulative_nb        |      25|   14.0208|               16.84|                   2.0|                 0.0|       0.68|
|Indiana              |compressed |cumulative_poisson   |      25|   12.8589|               15.36|                   3.0|                 0.0|       0.64|
|Indiana              |compressed |empirical_multiplier |      25|   20.1200|               20.12|                   1.0|                -1.0|       0.48|
|Indiana              |compressed |hurdle_ztnb          |      25|   21.5626|               22.28|                   3.0|                -3.0|       0.92|
|Indiana              |compressed |hurdle_ztpoisson     |      25|  104.6562|              146.34|                  57.0|               -57.0|       0.56|
|Iowa                 |calendar   |cumulative_nb        |      25|    3.9185|                4.92|                   1.0|                 0.0|       0.80|
|Iowa                 |calendar   |cumulative_poisson   |      25|    4.4007|                5.38|                   1.0|                 0.0|       0.72|
|Iowa                 |calendar   |empirical_multiplier |      25|    7.2800|                7.28|                   1.0|                -1.0|       0.44|
|Iowa                 |calendar   |hurdle_ztnb          |      25|    6.3520|                7.28|                   2.0|                 0.0|       0.80|
|Iowa                 |calendar   |hurdle_ztpoisson     |      25|    4.1625|                5.98|                   1.0|                -1.0|       0.88|
|Iowa                 |compressed |cumulative_nb        |      25|    4.5429|                5.56|                   1.0|                -1.0|       0.64|
|Iowa                 |compressed |cumulative_poisson   |      25|    5.0711|                5.80|                   1.0|                -1.0|       0.64|
|Iowa                 |compressed |empirical_multiplier |      25|    7.0800|                7.08|                   1.0|                -1.0|       0.44|
|Iowa                 |compressed |hurdle_ztnb          |      25|    6.3088|                7.14|                   2.0|                 0.0|       0.80|
|Iowa                 |compressed |hurdle_ztpoisson     |      25|    4.6805|                5.68|                   1.0|                 0.0|       0.76|
|Kansas               |calendar   |cumulative_nb        |      25|    2.6410|                3.46|                   1.0|                 0.0|       0.80|
|Kansas               |calendar   |cumulative_poisson   |      25|    3.0626|                3.78|                   1.0|                 0.0|       0.76|
|Kansas               |calendar   |empirical_multiplier |      25|    6.2400|                6.24|                   1.0|                -1.0|       0.40|
|Kansas               |calendar   |hurdle_ztnb          |      25|    5.1677|                6.22|                   2.0|                 0.0|       0.88|
|Kansas               |calendar   |hurdle_ztpoisson     |      25|    3.7912|                5.36|                   1.0|                -1.0|       0.88|
|Kansas               |compressed |cumulative_nb        |      25|   10.2486|               12.54|                   5.0|                -5.0|       0.52|
|Kansas               |compressed |cumulative_poisson   |      25|   11.6179|               14.06|                   8.0|                -8.0|       0.44|
|Kansas               |compressed |empirical_multiplier |      25|    6.2400|                6.24|                   1.0|                -1.0|       0.40|
|Kansas               |compressed |hurdle_ztnb          |      25|    5.3334|                6.38|                   2.0|                 0.0|       0.88|
|Kansas               |compressed |hurdle_ztpoisson     |      25|    6.0169|                9.16|                   2.0|                -1.0|       0.84|
|Kentucky             |calendar   |cumulative_nb        |      25|   11.7646|               14.30|                   6.0|                -6.0|       0.56|
|Kentucky             |calendar   |cumulative_poisson   |      25|   12.5197|               14.70|                   6.0|                -6.0|       0.56|
|Kentucky             |calendar   |empirical_multiplier |      25|   20.6800|               20.68|                   8.0|                -8.0|       0.24|
|Kentucky             |calendar   |hurdle_ztnb          |      25|   17.2175|               20.28|                   7.0|                -7.0|       0.68|
|Kentucky             |calendar   |hurdle_ztpoisson     |      25|   16.7430|               20.10|                   7.0|                -7.0|       0.56|
|Kentucky             |compressed |cumulative_nb        |      25|   18.9086|               22.32|                   8.0|                -8.0|       0.48|
|Kentucky             |compressed |cumulative_poisson   |      25|   19.0751|               22.32|                   8.0|                -8.0|       0.48|
|Kentucky             |compressed |empirical_multiplier |      25|   21.6000|               21.60|                   8.0|                -8.0|       0.28|
|Kentucky             |compressed |hurdle_ztnb          |      25|   19.5712|               21.16|                   6.0|                -6.0|       0.64|
|Kentucky             |compressed |hurdle_ztpoisson     |      25|   19.1427|               22.96|                   8.0|                -8.0|       0.52|
|Louisiana            |calendar   |cumulative_nb        |      25|   14.8537|               17.78|                  12.0|                 6.0|       0.44|
|Louisiana            |calendar   |cumulative_poisson   |      25|   16.1038|               18.88|                  13.0|                 6.0|       0.48|
|Louisiana            |calendar   |empirical_multiplier |      25|   17.0400|               17.04|                   9.0|                 0.0|       0.20|
|Louisiana            |calendar   |hurdle_ztnb          |      25|   16.2224|               17.58|                  10.0|                 2.0|       0.48|
|Louisiana            |calendar   |hurdle_ztpoisson     |      25|   16.0514|               17.00|                   9.0|                 3.0|       0.32|
|Louisiana            |compressed |cumulative_nb        |      25|   16.1239|               18.00|                  10.0|                 3.0|       0.28|
|Louisiana            |compressed |cumulative_poisson   |      25|   16.1637|               18.06|                  10.5|                 3.0|       0.32|
|Louisiana            |compressed |empirical_multiplier |      25|   17.3200|               17.32|                   9.0|                 3.0|       0.20|
|Louisiana            |compressed |hurdle_ztnb          |      25|   16.6951|               17.68|                  10.0|                 3.0|       0.56|
|Louisiana            |compressed |hurdle_ztpoisson     |      25|   13.3232|               14.84|                   5.0|                 0.0|       0.64|
|Maine                |calendar   |cumulative_nb        |      25|    1.7337|                2.56|                   1.0|                 0.0|       0.84|
|Maine                |calendar   |cumulative_poisson   |      25|    1.7173|                2.52|                   1.0|                 0.0|       0.88|
|Maine                |calendar   |empirical_multiplier |      25|    1.4800|                1.48|                   0.0|                 0.0|       0.68|
|Maine                |calendar   |hurdle_ztnb          |      25|    1.3198|                1.52|                   0.0|                 0.0|       0.80|
|Maine                |calendar   |hurdle_ztpoisson     |      25|    1.1740|                1.30|                   0.0|                 0.0|       0.84|
|Maine                |compressed |cumulative_nb        |      25|    1.3845|                1.72|                   0.0|                 0.0|       0.84|
|Maine                |compressed |cumulative_poisson   |      25|    1.2984|                1.70|                   0.0|                 0.0|       0.88|
|Maine                |compressed |empirical_multiplier |      25|    1.4400|                1.44|                   0.0|                 0.0|       0.72|
|Maine                |compressed |hurdle_ztnb          |      25|    1.3213|                1.48|                   0.0|                 0.0|       0.88|
|Maine                |compressed |hurdle_ztpoisson     |      25|    1.0965|                1.40|                   0.0|                 0.0|       0.88|
|Maryland             |calendar   |cumulative_nb        |      25|   11.3372|               13.24|                   2.0|                -1.0|       0.64|
|Maryland             |calendar   |cumulative_poisson   |      25|   12.3194|               14.16|                   4.0|                -1.0|       0.60|
|Maryland             |calendar   |empirical_multiplier |      25|   18.4000|               18.40|                   1.0|                -1.0|       0.40|
|Maryland             |calendar   |hurdle_ztnb          |      25|   16.3725|               18.30|                   1.0|                -1.0|       0.72|
|Maryland             |calendar   |hurdle_ztpoisson     |      25|   16.4056|               19.40|                   3.0|                -1.0|       0.60|
|Maryland             |compressed |cumulative_nb        |      25|   14.4116|               16.80|                   1.0|                -1.0|       0.64|
|Maryland             |compressed |cumulative_poisson   |      25|   14.6866|               16.88|                   1.0|                -1.0|       0.72|
|Maryland             |compressed |empirical_multiplier |      25|   17.9600|               17.96|                   1.0|                -1.0|       0.48|
|Maryland             |compressed |hurdle_ztnb          |      25|   16.7130|               18.08|                   1.0|                 0.0|       0.76|
|Maryland             |compressed |hurdle_ztpoisson     |      25|   15.0916|               16.66|                   1.0|                -1.0|       0.72|
|Massachusetts        |calendar   |cumulative_nb        |      25|   40.0671|               45.26|                  14.0|               -14.0|       0.28|
|Massachusetts        |calendar   |cumulative_poisson   |      25|   41.8494|               47.34|                  14.0|               -14.0|       0.32|
|Massachusetts        |calendar   |empirical_multiplier |      25|   37.9200|               37.92|                   9.0|                -9.0|       0.20|
|Massachusetts        |calendar   |hurdle_ztnb          |      25|   36.9863|               37.92|                   9.0|                -9.0|       0.44|
|Massachusetts        |calendar   |hurdle_ztpoisson     |      25|   45.8987|               53.44|                  15.0|               -15.0|       0.24|
|Massachusetts        |compressed |cumulative_nb        |      25|   34.2669|               36.24|                   8.0|                -8.0|       0.40|
|Massachusetts        |compressed |cumulative_poisson   |      25|   34.9077|               36.72|                   8.0|                -8.0|       0.40|
|Massachusetts        |compressed |empirical_multiplier |      25|   36.7600|               36.76|                   8.0|                -8.0|       0.24|
|Massachusetts        |compressed |hurdle_ztnb          |      25|   33.7338|               36.92|                   8.0|                -8.0|       0.92|
|Massachusetts        |compressed |hurdle_ztpoisson     |      25|   52.6325|               67.96|                  24.0|               -24.0|       0.48|
|Michigan             |calendar   |cumulative_nb        |      25|   92.8772|               98.06|                  52.0|               -52.0|       0.16|
|Michigan             |calendar   |cumulative_poisson   |      25|  111.6349|              118.78|                  60.5|               -60.5|       0.20|
|Michigan             |calendar   |empirical_multiplier |      25|  116.6400|              116.64|                  59.0|               -59.0|       0.00|
|Michigan             |calendar   |hurdle_ztnb          |      25|  106.2160|              116.44|                  59.0|               -59.0|       0.52|
|Michigan             |calendar   |hurdle_ztpoisson     |      25|  112.2454|              116.64|                  59.0|               -59.0|       0.12|
|Michigan             |compressed |cumulative_nb        |      25|  108.7127|              111.48|                  57.0|               -57.0|       0.16|
|Michigan             |compressed |cumulative_poisson   |      25|  109.3278|              111.64|                  57.0|               -57.0|       0.16|
|Michigan             |compressed |empirical_multiplier |      25|  117.3200|              117.32|                  59.0|               -59.0|       0.00|
|Michigan             |compressed |hurdle_ztnb          |      25|  101.2233|              116.68|                  59.0|               -59.0|       0.56|
|Michigan             |compressed |hurdle_ztpoisson     |      25|  152.5399|              179.60|                 108.0|              -108.0|       0.20|
|Minnesota            |calendar   |cumulative_nb        |      25|   17.1376|               19.86|                   5.0|                -1.0|       0.72|
|Minnesota            |calendar   |cumulative_poisson   |      25|   21.1895|               23.74|                   4.0|                 0.0|       0.60|
|Minnesota            |calendar   |empirical_multiplier |      25|   15.0800|               15.08|                   0.0|                 0.0|       0.52|
|Minnesota            |calendar   |hurdle_ztnb          |      25|   16.1283|               15.08|                   0.0|                 0.0|       0.72|
|Minnesota            |calendar   |hurdle_ztpoisson     |      25|   12.7024|               15.08|                   0.0|                 0.0|       0.80|
|Minnesota            |compressed |cumulative_nb        |      25|   16.1360|               17.56|                   2.0|                 0.0|       0.64|
|Minnesota            |compressed |cumulative_poisson   |      25|   16.0136|               17.34|                   1.0|                 0.0|       0.64|
|Minnesota            |compressed |empirical_multiplier |      25|   15.0400|               15.04|                   0.0|                 0.0|       0.52|
|Minnesota            |compressed |hurdle_ztnb          |      25|   15.8239|               15.02|                   0.5|                 0.0|       0.68|
|Minnesota            |compressed |hurdle_ztpoisson     |      25|   14.1565|               15.04|                   0.0|                 0.0|       0.68|
|Mississippi          |calendar   |cumulative_nb        |      25|    9.5172|               11.94|                   2.0|                 2.0|       0.60|
|Mississippi          |calendar   |cumulative_poisson   |      25|    4.2556|                5.42|                   1.0|                 1.0|       0.84|
|Mississippi          |calendar   |empirical_multiplier |      25|    3.3200|                3.32|                   0.0|                 0.0|       0.52|
|Mississippi          |calendar   |hurdle_ztnb          |      25|    2.9765|                3.16|                   1.0|                 0.0|       0.92|
|Mississippi          |calendar   |hurdle_ztpoisson     |      25|    2.9384|                3.44|                   1.0|                 0.0|       0.92|
|Mississippi          |compressed |cumulative_nb        |      25|    4.2484|                5.16|                   2.0|                -2.0|       0.56|
|Mississippi          |compressed |cumulative_poisson   |      25|    4.2799|                5.16|                   2.0|                -2.0|       0.56|
|Mississippi          |compressed |empirical_multiplier |      25|    3.2800|                3.28|                   0.0|                 0.0|       0.52|
|Mississippi          |compressed |hurdle_ztnb          |      25|    3.0648|                3.24|                   1.0|                 0.0|       0.88|
|Mississippi          |compressed |hurdle_ztpoisson     |      25|    3.2284|                3.74|                   1.0|                 0.0|       0.88|
|Missouri             |calendar   |cumulative_nb        |      25|   24.2695|               26.64|                   3.0|                 0.0|       0.76|
|Missouri             |calendar   |cumulative_poisson   |      25|   24.4844|               27.10|                   4.0|                -4.0|       0.64|
|Missouri             |calendar   |empirical_multiplier |      25|   35.2000|               35.20|                   1.0|                -1.0|       0.48|
|Missouri             |calendar   |hurdle_ztnb          |      25|   30.3549|               34.26|                   3.0|                 0.0|       0.88|
|Missouri             |calendar   |hurdle_ztpoisson     |      25|   30.3818|               34.36|                   4.0|                 0.0|       0.76|
|Missouri             |compressed |cumulative_nb        |      25|   31.6316|               33.98|                   5.0|                -5.0|       0.64|
|Missouri             |compressed |cumulative_poisson   |      25|   32.0148|               34.50|                   4.0|                -4.0|       0.56|
|Missouri             |compressed |empirical_multiplier |      25|   35.6400|               35.64|                   1.0|                -1.0|       0.48|
|Missouri             |compressed |hurdle_ztnb          |      25|   30.2952|               33.52|                   4.0|                 0.0|       0.84|
|Missouri             |compressed |hurdle_ztpoisson     |      25|   28.5521|               32.46|                   3.0|                 0.0|       0.72|
|Montana              |calendar   |cumulative_nb        |      25|    2.7887|                4.08|                   4.0|                 0.0|       0.76|
|Montana              |calendar   |cumulative_poisson   |      25|    2.7987|                3.92|                   4.0|                 0.0|       0.68|
|Montana              |calendar   |empirical_multiplier |      25|    4.2400|                4.24|                   3.0|                 0.0|       0.28|
|Montana              |calendar   |hurdle_ztnb          |      25|    3.7762|                4.24|                   3.0|                 0.0|       0.48|
|Montana              |calendar   |hurdle_ztpoisson     |      25|    3.4393|                4.60|                   4.0|                 0.0|       0.52|
|Montana              |compressed |cumulative_nb        |      25|    3.0528|                4.26|                   5.0|                 0.0|       0.76|
|Montana              |compressed |cumulative_poisson   |      25|    3.0864|                4.28|                   4.0|                 0.0|       0.76|
|Montana              |compressed |empirical_multiplier |      25|    4.3600|                4.36|                   3.0|                 0.0|       0.28|
|Montana              |compressed |hurdle_ztnb          |      25|    3.8408|                4.48|                   4.0|                 0.0|       0.64|
|Montana              |compressed |hurdle_ztpoisson     |      25|    3.3938|                4.40|                   5.0|                 0.0|       0.60|
|Nebraska             |calendar   |cumulative_nb        |      25|    1.8961|                2.68|                   0.0|                 0.0|       0.96|
|Nebraska             |calendar   |cumulative_poisson   |      25|    2.1772|                3.10|                   1.0|                 0.0|       0.92|
|Nebraska             |calendar   |empirical_multiplier |      25|    1.7200|                1.72|                   0.0|                 0.0|       0.68|
|Nebraska             |calendar   |hurdle_ztnb          |      25|    1.5438|                1.84|                   0.0|                 0.0|       0.88|
|Nebraska             |calendar   |hurdle_ztpoisson     |      25|    1.0614|                1.32|                   0.0|                 0.0|       0.92|
|Nebraska             |compressed |cumulative_nb        |      25|    1.2475|                1.54|                   0.0|                 0.0|       0.96|
|Nebraska             |compressed |cumulative_poisson   |      25|    1.3247|                1.62|                   0.0|                 0.0|       0.96|
|Nebraska             |compressed |empirical_multiplier |      25|    1.5200|                1.52|                   0.0|                 0.0|       0.76|
|Nebraska             |compressed |hurdle_ztnb          |      25|    1.4844|                1.68|                   0.0|                 0.0|       0.80|
|Nebraska             |compressed |hurdle_ztpoisson     |      25|    1.1295|                1.28|                   0.0|                 0.0|       0.92|
|Nevada               |calendar   |cumulative_nb        |      25|    3.3768|                4.86|                   2.0|                 0.0|       0.80|
|Nevada               |calendar   |cumulative_poisson   |      25|    4.3679|                5.84|                   2.0|                 0.0|       0.60|
|Nevada               |calendar   |empirical_multiplier |      25|    4.0000|                4.00|                   0.0|                 0.0|       0.52|
|Nevada               |calendar   |hurdle_ztnb          |      25|    3.4091|                3.96|                   1.0|                 0.0|       0.84|
|Nevada               |calendar   |hurdle_ztpoisson     |      25|    2.7933|                3.80|                   0.0|                 0.0|       0.84|
|Nevada               |compressed |cumulative_nb        |      25|    3.9046|                4.62|                   1.0|                 0.0|       0.76|
|Nevada               |compressed |cumulative_poisson   |      25|    3.8591|                4.56|                   1.0|                -1.0|       0.76|
|Nevada               |compressed |empirical_multiplier |      25|    4.0000|                4.00|                   0.0|                 0.0|       0.52|
|Nevada               |compressed |hurdle_ztnb          |      25|    3.5447|                3.96|                   0.0|                 0.0|       0.80|
|Nevada               |compressed |hurdle_ztpoisson     |      25|    3.1602|                4.14|                   1.0|                 0.0|       0.92|
|New Hampshire        |calendar   |cumulative_nb        |      25|   13.1168|               14.38|                   1.0|                 0.0|       0.72|
|New Hampshire        |calendar   |cumulative_poisson   |      25|   14.2519|               15.66|                   1.0|                -1.0|       0.68|
|New Hampshire        |calendar   |empirical_multiplier |      25|   12.6400|               12.64|                   0.0|                 0.0|       0.64|
|New Hampshire        |calendar   |hurdle_ztnb          |      25|   12.4866|               12.68|                   0.0|                 0.0|       0.68|
|New Hampshire        |calendar   |hurdle_ztpoisson     |      25|   11.6686|               12.64|                   0.0|                 0.0|       0.72|
|New Hampshire        |compressed |cumulative_nb        |      25|   13.2873|               14.26|                   1.0|                 0.0|       0.64|
|New Hampshire        |compressed |cumulative_poisson   |      25|   13.4644|               14.46|                   1.0|                 0.0|       0.64|
|New Hampshire        |compressed |empirical_multiplier |      25|   13.2000|               13.20|                   0.0|                 0.0|       0.60|
|New Hampshire        |compressed |hurdle_ztnb          |      25|   13.3691|               13.60|                   0.0|                 0.0|       0.64|
|New Hampshire        |compressed |hurdle_ztpoisson     |      25|   13.3058|               13.94|                   0.0|                 0.0|       0.60|
|New Jersey           |calendar   |cumulative_nb        |      25|  103.7491|              125.16|                  52.5|                52.5|       0.00|
|New Jersey           |calendar   |cumulative_poisson   |      25|   15.9204|               17.30|                   3.0|                 0.0|       0.60|
|New Jersey           |calendar   |empirical_multiplier |      25|    1.8400|                1.84|                   0.0|                 0.0|       0.72|
|New Jersey           |calendar   |hurdle_ztnb          |      25|    2.7913|                1.90|                   0.0|                 0.0|       1.00|
|New Jersey           |calendar   |hurdle_ztpoisson     |      25|    2.2518|                1.84|                   0.0|                 0.0|       0.72|
|New Jersey           |compressed |cumulative_nb        |      25|    3.5136|                3.44|                   2.0|                -1.0|       0.72|
|New Jersey           |compressed |cumulative_poisson   |      25|    3.6012|                3.62|                   1.5|                -1.0|       0.72|
|New Jersey           |compressed |empirical_multiplier |      25|    1.8400|                1.84|                   0.0|                 0.0|       0.72|
|New Jersey           |compressed |hurdle_ztnb          |      25|    2.3289|                1.84|                   0.0|                 0.0|       0.80|
|New Jersey           |compressed |hurdle_ztpoisson     |      25|    2.7874|                2.36|                   0.0|                 0.0|       0.80|
|New Mexico           |calendar   |cumulative_nb        |      25|    5.8481|                7.02|                   1.5|                -1.0|       0.88|
|New Mexico           |calendar   |cumulative_poisson   |      25|    5.9217|                7.28|                   2.0|                -1.0|       0.88|
|New Mexico           |calendar   |empirical_multiplier |      25|    9.2400|                9.24|                   2.0|                -2.0|       0.36|
|New Mexico           |calendar   |hurdle_ztnb          |      25|    7.0893|                8.96|                   2.0|                -1.0|       0.92|
|New Mexico           |calendar   |hurdle_ztpoisson     |      25|    7.8293|                8.88|                   2.0|                -1.0|       0.80|
|New Mexico           |compressed |cumulative_nb        |      25|    7.3631|                8.40|                   1.0|                -1.0|       0.80|
|New Mexico           |compressed |cumulative_poisson   |      25|    7.4231|                8.42|                   1.0|                -1.0|       0.76|
|New Mexico           |compressed |empirical_multiplier |      25|    9.2000|                9.20|                   2.0|                -2.0|       0.40|
|New Mexico           |compressed |hurdle_ztnb          |      25|    7.2819|                8.74|                   2.0|                -1.0|       0.76|
|New Mexico           |compressed |hurdle_ztpoisson     |      25|    6.8802|                7.96|                   1.0|                -1.0|       0.80|
|New York             |calendar   |cumulative_nb        |      25|   16.4688|               24.18|                   9.0|                 2.0|       0.84|
|New York             |calendar   |cumulative_poisson   |      25|   30.7758|               34.84|                   5.0|                 0.0|       0.68|
|New York             |calendar   |empirical_multiplier |      25|   29.3600|               29.36|                   8.0|                -8.0|       0.24|
|New York             |calendar   |hurdle_ztnb          |      25|   21.8359|               27.90|                   7.0|                -1.0|       0.84|
|New York             |calendar   |hurdle_ztpoisson     |      25|   27.3913|               29.40|                   9.0|                -8.0|       0.48|
|New York             |compressed |cumulative_nb        |      25|   16.5329|               18.76|                   2.5|                -2.0|       0.68|
|New York             |compressed |cumulative_poisson   |      25|   17.5432|               20.12|                   3.0|                -1.0|       0.64|
|New York             |compressed |empirical_multiplier |      25|   27.3200|               27.32|                   7.0|                -7.0|       0.32|
|New York             |compressed |hurdle_ztnb          |      25|   20.2927|               24.88|                   7.0|                 0.0|       0.88|
|New York             |compressed |hurdle_ztpoisson     |      25|   15.6469|               21.74|                   3.0|                -1.0|       0.76|
|North Carolina       |calendar   |cumulative_nb        |      25|   17.8564|               24.22|                   4.0|                 4.0|       0.64|
|North Carolina       |calendar   |cumulative_poisson   |      25|   15.7890|               18.92|                   2.0|                 0.0|       0.72|
|North Carolina       |calendar   |empirical_multiplier |      25|   22.1200|               22.12|                   1.0|                -1.0|       0.44|
|North Carolina       |calendar   |hurdle_ztnb          |      25|   18.2688|               22.08|                   2.0|                -1.0|       0.84|
|North Carolina       |calendar   |hurdle_ztpoisson     |      25|   21.3864|               22.12|                   1.0|                -1.0|       0.48|
|North Carolina       |compressed |cumulative_nb        |      25|   17.7035|               20.66|                   1.0|                -1.0|       0.60|
|North Carolina       |compressed |cumulative_poisson   |      25|   17.7556|               20.86|                   1.0|                -1.0|       0.60|
|North Carolina       |compressed |empirical_multiplier |      25|   22.0000|               22.00|                   1.0|                -1.0|       0.44|
|North Carolina       |compressed |hurdle_ztnb          |      25|   20.0588|               22.00|                   2.0|                -1.0|       0.68|
|North Carolina       |compressed |hurdle_ztpoisson     |      25|   19.1369|               21.56|                   2.0|                -2.0|       0.64|
|North Dakota         |calendar   |cumulative_nb        |      25|    0.7241|                0.82|                   0.0|                 0.0|       0.96|
|North Dakota         |calendar   |cumulative_poisson   |      25|    0.7150|                0.76|                   0.0|                 0.0|       0.96|
|North Dakota         |calendar   |empirical_multiplier |      25|    0.7600|                0.76|                   0.0|                 0.0|       0.80|
|North Dakota         |calendar   |hurdle_ztnb          |      25|    0.6004|                0.76|                   0.0|                 0.0|       0.96|
|North Dakota         |calendar   |hurdle_ztpoisson     |      25|    0.7071|                0.72|                   0.0|                 0.0|       0.96|
|North Dakota         |compressed |cumulative_nb        |      25|    0.6017|                0.56|                   0.0|                 0.0|       0.96|
|North Dakota         |compressed |cumulative_poisson   |      25|    0.6308|                0.64|                   0.0|                 0.0|       0.96|
|North Dakota         |compressed |empirical_multiplier |      25|    0.7600|                0.76|                   0.0|                 0.0|       0.80|
|North Dakota         |compressed |hurdle_ztnb          |      25|    0.6029|                0.76|                   0.0|                 0.0|       0.96|
|North Dakota         |compressed |hurdle_ztpoisson     |      25|    0.5608|                0.52|                   0.0|                 0.0|       0.96|
|Ohio                 |calendar   |cumulative_nb        |      25|   21.7072|               29.62|                   8.0|                -3.0|       0.80|
|Ohio                 |calendar   |cumulative_poisson   |      25|   28.2841|               31.90|                   4.0|                -1.0|       0.60|
|Ohio                 |calendar   |empirical_multiplier |      25|   33.3600|               33.36|                   5.0|                 0.0|       0.20|
|Ohio                 |calendar   |hurdle_ztnb          |      25|   26.7008|               33.30|                   6.0|                 1.0|       0.72|
|Ohio                 |calendar   |hurdle_ztpoisson     |      25|   22.7263|               31.86|                   6.0|                -0.5|       0.72|
|Ohio                 |compressed |cumulative_nb        |      25|   31.4761|               36.34|                   5.0|                -1.0|       0.60|
|Ohio                 |compressed |cumulative_poisson   |      25|   27.5394|               29.92|                   5.0|                 0.0|       0.32|
|Ohio                 |compressed |empirical_multiplier |      25|   35.3600|               35.36|                   6.0|                -2.0|       0.20|
|Ohio                 |compressed |hurdle_ztnb          |      25|   31.3715|               34.40|                   5.0|                 0.0|       0.76|
|Ohio                 |compressed |hurdle_ztpoisson     |      25|   20.6972|               29.56|                   4.0|                -3.0|       0.72|
|Oklahoma             |calendar   |cumulative_nb        |      25|   13.7129|               15.82|                   2.0|                -2.0|       0.68|
|Oklahoma             |calendar   |cumulative_poisson   |      25|   15.5848|               17.32|                   4.0|                -4.0|       0.52|
|Oklahoma             |calendar   |empirical_multiplier |      25|   20.1200|               20.12|                   1.0|                 0.0|       0.40|
|Oklahoma             |calendar   |hurdle_ztnb          |      25|   18.0847|               19.76|                   1.0|                 0.0|       0.84|
|Oklahoma             |calendar   |hurdle_ztpoisson     |      25|   17.0431|               20.24|                   2.0|                 0.0|       0.64|
|Oklahoma             |compressed |cumulative_nb        |      25|   17.0651|               20.38|                   4.0|                -4.0|       0.52|
|Oklahoma             |compressed |cumulative_poisson   |      25|   18.1864|               20.74|                   4.0|                -4.0|       0.52|
|Oklahoma             |compressed |empirical_multiplier |      25|   20.1200|               20.12|                   1.0|                 0.0|       0.40|
|Oklahoma             |compressed |hurdle_ztnb          |      25|   18.2859|               19.72|                   1.0|                 0.0|       0.88|
|Oklahoma             |compressed |hurdle_ztpoisson     |      25|   17.0627|               19.78|                   1.0|                -1.0|       0.68|
|Oregon               |calendar   |cumulative_nb        |      25|    6.7617|                8.78|                   3.0|                 0.0|       0.72|
|Oregon               |calendar   |cumulative_poisson   |      25|    7.7306|                8.90|                   2.0|                -1.0|       0.52|
|Oregon               |calendar   |empirical_multiplier |      25|   10.4000|               10.40|                   2.0|                -2.0|       0.12|
|Oregon               |calendar   |hurdle_ztnb          |      25|    7.6120|                9.88|                   2.0|                -2.0|       0.84|
|Oregon               |calendar   |hurdle_ztpoisson     |      25|    7.6671|                9.68|                   3.0|                -2.0|       0.68|
|Oregon               |compressed |cumulative_nb        |      25|    8.1272|                9.32|                   3.0|                -3.0|       0.36|
|Oregon               |compressed |cumulative_poisson   |      25|    8.2362|                9.46|                   4.0|                -4.0|       0.40|
|Oregon               |compressed |empirical_multiplier |      25|   10.5200|               10.52|                   3.0|                -3.0|       0.12|
|Oregon               |compressed |hurdle_ztnb          |      25|    8.3980|                9.96|                   2.0|                -2.0|       0.60|
|Oregon               |compressed |hurdle_ztpoisson     |      25|    7.5203|                9.76|                   2.0|                -2.0|       0.44|
|Pennsylvania         |calendar   |cumulative_nb        |      25|  238.9809|              329.02|                 138.5|               138.5|       0.28|
|Pennsylvania         |calendar   |cumulative_poisson   |      25|   64.2215|               69.78|                  24.0|               -23.0|       0.24|
|Pennsylvania         |calendar   |empirical_multiplier |      25|   88.9200|               88.92|                  70.0|               -70.0|       0.00|
|Pennsylvania         |calendar   |hurdle_ztnb          |      25|   62.5615|               87.10|                  68.0|               -68.0|       0.80|
|Pennsylvania         |calendar   |hurdle_ztpoisson     |      25|   86.6454|               88.92|                  70.0|               -70.0|       0.08|
|Pennsylvania         |compressed |cumulative_nb        |      25|  105.6569|              114.72|                  82.0|               -82.0|       0.16|
|Pennsylvania         |compressed |cumulative_poisson   |      25|  120.8222|              129.14|                  84.0|               -84.0|       0.16|
|Pennsylvania         |compressed |empirical_multiplier |      25|  106.2400|              106.24|                  73.0|               -73.0|       0.04|
|Pennsylvania         |compressed |hurdle_ztnb          |      25|  100.0690|              106.40|                  73.0|               -73.0|       0.36|
|Pennsylvania         |compressed |hurdle_ztpoisson     |      25|  108.9585|              117.70|                  82.5|               -82.5|       0.20|
|Puerto Rico          |calendar   |cumulative_nb        |      25|  105.0203|              111.86|                  58.0|                 4.0|       0.20|
|Puerto Rico          |calendar   |cumulative_poisson   |      25|  107.2883|              111.78|                  45.5|                -2.0|       0.16|
|Puerto Rico          |calendar   |empirical_multiplier |      25|  103.4000|              103.40|                  26.0|               -26.0|       0.08|
|Puerto Rico          |calendar   |hurdle_ztnb          |      25|   93.1469|              100.84|                  24.0|               -22.0|       0.68|
|Puerto Rico          |calendar   |hurdle_ztpoisson     |      25|  106.2246|              120.14|                  66.5|               -66.5|       0.48|
|Puerto Rico          |compressed |cumulative_nb        |      25|   96.3312|               98.96|                  23.0|               -23.0|       0.40|
|Puerto Rico          |compressed |cumulative_poisson   |      25|  101.0280|              103.80|                  24.0|                -9.0|       0.12|
|Puerto Rico          |compressed |empirical_multiplier |      25|   99.4800|               99.48|                  26.0|               -26.0|       0.12|
|Puerto Rico          |compressed |hurdle_ztnb          |      25|   94.5637|               97.96|                  20.0|               -20.0|       0.72|
|Puerto Rico          |compressed |hurdle_ztpoisson     |      25|  115.2630|              134.02|                  95.0|               -95.0|       0.40|
|Rhode Island         |calendar   |cumulative_nb        |      25|    3.5356|                4.36|                   0.0|                 0.0|       0.76|
|Rhode Island         |calendar   |cumulative_poisson   |      25|    3.7957|                4.80|                   0.0|                 0.0|       0.76|
|Rhode Island         |calendar   |empirical_multiplier |      25|    4.0400|                4.04|                   0.0|                 0.0|       0.64|
|Rhode Island         |calendar   |hurdle_ztnb          |      25|    3.6319|                4.04|                   0.0|                 0.0|       0.72|
|Rhode Island         |calendar   |hurdle_ztpoisson     |      25|    3.2296|                3.72|                   0.0|                 0.0|       0.76|
|Rhode Island         |compressed |cumulative_nb        |      25|    3.7345|                4.20|                   0.0|                 0.0|       0.72|
|Rhode Island         |compressed |cumulative_poisson   |      25|    3.6415|                4.32|                   0.0|                 0.0|       0.72|
|Rhode Island         |compressed |empirical_multiplier |      25|    4.0400|                4.04|                   0.0|                 0.0|       0.64|
|Rhode Island         |compressed |hurdle_ztnb          |      25|    3.9549|                4.04|                   0.0|                 0.0|       0.72|
|Rhode Island         |compressed |hurdle_ztpoisson     |      25|    3.8300|                4.48|                   0.0|                 0.0|       0.76|
|South Carolina       |calendar   |cumulative_nb        |      25|   44.0240|               55.22|                  15.5|                15.5|       0.32|
|South Carolina       |calendar   |cumulative_poisson   |      25|    9.1384|               10.64|                   1.0|                 0.0|       0.72|
|South Carolina       |calendar   |empirical_multiplier |      25|    7.9200|                7.92|                   3.0|                -3.0|       0.40|
|South Carolina       |calendar   |hurdle_ztnb          |      25|    6.3121|                7.72|                   3.0|                -2.0|       0.92|
|South Carolina       |calendar   |hurdle_ztpoisson     |      25|    6.8257|                7.92|                   3.0|                -3.0|       0.56|
|South Carolina       |compressed |cumulative_nb        |      25|    6.5889|                7.26|                   3.0|                -2.0|       0.52|
|South Carolina       |compressed |cumulative_poisson   |      25|    6.7250|                7.36|                   3.0|                -2.0|       0.56|
|South Carolina       |compressed |empirical_multiplier |      25|    6.4000|                6.40|                   3.0|                -3.0|       0.40|
|South Carolina       |compressed |hurdle_ztnb          |      25|    5.1179|                6.16|                   3.0|                -3.0|       0.92|
|South Carolina       |compressed |hurdle_ztpoisson     |      25|    5.3581|                7.10|                   2.0|                -2.0|       0.84|
|South Dakota         |calendar   |cumulative_nb        |      25|    1.2295|                1.48|                   0.0|                 0.0|       0.92|
|South Dakota         |calendar   |cumulative_poisson   |      25|    1.2299|                1.48|                   0.0|                 0.0|       0.92|
|South Dakota         |calendar   |empirical_multiplier |      25|    1.8000|                1.80|                   0.0|                 0.0|       0.60|
|South Dakota         |calendar   |hurdle_ztnb          |      25|    1.5752|                1.80|                   0.0|                 0.0|       0.80|
|South Dakota         |calendar   |hurdle_ztpoisson     |      25|    1.0260|                1.36|                   0.0|                 0.0|       0.88|
|South Dakota         |compressed |cumulative_nb        |      25|    1.4332|                1.94|                   0.0|                 0.0|       0.88|
|South Dakota         |compressed |cumulative_poisson   |      25|    1.4378|                2.00|                   0.0|                 0.0|       0.84|
|South Dakota         |compressed |empirical_multiplier |      25|    1.8400|                1.84|                   0.0|                 0.0|       0.60|
|South Dakota         |compressed |hurdle_ztnb          |      25|    1.8019|                1.80|                   0.0|                 0.0|       0.76|
|South Dakota         |compressed |hurdle_ztpoisson     |      25|    1.4717|                1.76|                   0.0|                 0.0|       0.76|
|Tennessee            |calendar   |cumulative_nb        |      25|   23.0011|               28.10|                   7.0|                 7.0|       0.40|
|Tennessee            |calendar   |cumulative_poisson   |      25|    9.9319|               11.60|                   2.0|                 0.0|       0.80|
|Tennessee            |calendar   |empirical_multiplier |      25|    6.7200|                6.72|                   0.0|                 0.0|       0.52|
|Tennessee            |calendar   |hurdle_ztnb          |      25|    5.5126|                7.00|                   2.0|                 0.0|       0.92|
|Tennessee            |calendar   |hurdle_ztpoisson     |      25|    4.9398|                6.34|                   0.0|                 0.0|       0.84|
|Tennessee            |compressed |cumulative_nb        |      25|    6.4737|                7.56|                   1.0|                 0.0|       0.68|
|Tennessee            |compressed |cumulative_poisson   |      25|    6.7039|                7.66|                   1.0|                 0.0|       0.68|
|Tennessee            |compressed |empirical_multiplier |      25|    6.6400|                6.64|                   0.0|                 0.0|       0.56|
|Tennessee            |compressed |hurdle_ztnb          |      25|    6.1463|                6.80|                   1.0|                 0.0|       0.84|
|Tennessee            |compressed |hurdle_ztpoisson     |      25|    5.1979|                6.60|                   0.0|                 0.0|       0.84|
|Texas                |calendar   |cumulative_nb        |      25|   25.0951|               39.98|                  12.0|                12.0|       0.84|
|Texas                |calendar   |cumulative_poisson   |      25|   45.8450|               52.52|                  16.0|               -13.0|       0.24|
|Texas                |calendar   |empirical_multiplier |      25|   34.6000|               34.60|                   9.0|                -9.0|       0.08|
|Texas                |calendar   |hurdle_ztnb          |      25|   24.2835|               31.02|                   5.5|                -5.5|       0.92|
|Texas                |calendar   |hurdle_ztpoisson     |      25|   30.9124|               34.14|                   9.0|                -9.0|       0.32|
|Texas                |compressed |cumulative_nb        |      25|   14.2796|               20.62|                  10.0|                 1.0|       0.72|
|Texas                |compressed |cumulative_poisson   |      25|   33.8104|               39.26|                  26.0|                15.0|       0.32|
|Texas                |compressed |empirical_multiplier |      25|   34.6800|               34.68|                   9.0|                -9.0|       0.08|
|Texas                |compressed |hurdle_ztnb          |      25|   29.4222|               35.30|                  20.5|                 1.0|       1.00|
|Texas                |compressed |hurdle_ztpoisson     |      25|   50.2315|               70.12|                  22.0|               -22.0|       0.36|
|US                   |calendar   |cumulative_nb        |      25| 1902.2665|             2910.24|                1596.5|              1596.5|       0.36|
|US                   |calendar   |cumulative_poisson   |      25|  737.4464|              767.60|                 251.0|              -176.0|       0.04|
|US                   |calendar   |empirical_multiplier |      25|  978.7200|              978.72|                 456.0|              -456.0|       0.00|
|US                   |calendar   |hurdle_ztnb          |      25|  732.8288|              929.72|                 403.5|              -403.5|       0.72|
|US                   |calendar   |hurdle_ztpoisson     |      25|  978.8734|              987.94|                 463.0|              -463.0|       0.00|
|US                   |compressed |cumulative_nb        |      25|  774.7850|              806.80|                 350.0|              -350.0|       0.00|
|US                   |compressed |cumulative_poisson   |      25|  671.8533|              694.92|                 185.0|              -185.0|       0.00|
|US                   |compressed |empirical_multiplier |      25|  981.3600|              981.36|                 456.0|              -456.0|       0.00|
|US                   |compressed |hurdle_ztnb          |      25|  736.7026|              925.04|                 332.5|              -332.5|       0.72|
|US                   |compressed |hurdle_ztpoisson     |      25|  986.5870|             1162.48|                 521.5|              -521.5|       0.00|
|Utah                 |calendar   |cumulative_nb        |      25|    3.8123|                4.84|                   1.0|                 0.0|       0.84|
|Utah                 |calendar   |cumulative_poisson   |      25|    5.6053|                7.10|                   1.5|                 0.0|       0.80|
|Utah                 |calendar   |empirical_multiplier |      25|    3.5600|                3.56|                   0.0|                 0.0|       0.72|
|Utah                 |calendar   |hurdle_ztnb          |      25|    3.5439|                3.56|                   0.0|                 0.0|       0.88|
|Utah                 |calendar   |hurdle_ztpoisson     |      25|    2.5581|                3.30|                   0.0|                 0.0|       0.92|
|Utah                 |compressed |cumulative_nb        |      25|    2.6467|                3.16|                   0.0|                 0.0|       0.92|
|Utah                 |compressed |cumulative_poisson   |      25|    2.5818|                3.32|                   0.0|                 0.0|       0.88|
|Utah                 |compressed |empirical_multiplier |      25|    2.8000|                2.80|                   0.0|                 0.0|       0.76|
|Utah                 |compressed |hurdle_ztnb          |      25|    2.7009|                2.80|                   0.0|                 0.0|       0.88|
|Utah                 |compressed |hurdle_ztpoisson     |      25|    2.2075|                2.60|                   0.0|                 0.0|       0.88|
|Vermont              |calendar   |cumulative_nb        |      25|    0.9638|                1.28|                   0.0|                 0.0|       0.88|
|Vermont              |calendar   |cumulative_poisson   |      25|    0.9461|                1.28|                   0.0|                 0.0|       0.96|
|Vermont              |calendar   |empirical_multiplier |      25|    1.4000|                1.40|                   0.0|                 0.0|       0.72|
|Vermont              |calendar   |hurdle_ztnb          |      25|    1.3175|                1.40|                   0.0|                 0.0|       0.84|
|Vermont              |calendar   |hurdle_ztpoisson     |      25|    0.7206|                1.12|                   0.0|                 0.0|       0.96|
|Vermont              |compressed |cumulative_nb        |      25|    1.0689|                1.40|                   0.0|                 0.0|       0.92|
|Vermont              |compressed |cumulative_poisson   |      25|    1.1148|                1.42|                   0.0|                 0.0|       0.92|
|Vermont              |compressed |empirical_multiplier |      25|    1.3200|                1.32|                   0.0|                 0.0|       0.76|
|Vermont              |compressed |hurdle_ztnb          |      25|    1.3047|                1.36|                   0.0|                 0.0|       0.80|
|Vermont              |compressed |hurdle_ztpoisson     |      25|    0.9708|                1.24|                   0.0|                 0.0|       0.88|
|Virginia             |calendar   |cumulative_nb        |      25|   66.2710|               84.24|                  41.0|                41.0|       0.20|
|Virginia             |calendar   |cumulative_poisson   |      25|   15.6377|               17.78|                   6.0|                -2.0|       0.36|
|Virginia             |calendar   |empirical_multiplier |      25|   22.4400|               22.44|                   8.0|                -8.0|       0.24|
|Virginia             |calendar   |hurdle_ztnb          |      25|   17.5874|               21.66|                   7.5|                -5.0|       0.88|
|Virginia             |calendar   |hurdle_ztpoisson     |      25|   19.2582|               22.10|                   8.0|                -8.0|       0.60|
|Virginia             |compressed |cumulative_nb        |      25|   19.8221|               22.30|                   8.0|                -8.0|       0.48|
|Virginia             |compressed |cumulative_poisson   |      25|   19.6630|               21.88|                   8.0|                -8.0|       0.48|
|Virginia             |compressed |empirical_multiplier |      25|   21.8400|               21.84|                   8.0|                -8.0|       0.28|
|Virginia             |compressed |hurdle_ztnb          |      25|   20.7052|               21.60|                   8.0|                -8.0|       0.72|
|Virginia             |compressed |hurdle_ztpoisson     |      25|   20.4455|               23.08|                   8.0|                -8.0|       0.40|
|Washington           |calendar   |cumulative_nb        |      25|   14.0161|               19.96|                   8.0|                 8.0|       0.64|
|Washington           |calendar   |cumulative_poisson   |      25|    5.5927|                7.00|                   3.0|                -2.0|       0.64|
|Washington           |calendar   |empirical_multiplier |      25|    7.0000|                7.00|                   2.0|                 0.0|       0.36|
|Washington           |calendar   |hurdle_ztnb          |      25|    5.7047|                6.90|                   2.0|                 0.0|       1.00|
|Washington           |calendar   |hurdle_ztpoisson     |      25|    5.9868|                7.00|                   2.0|                 0.0|       0.72|
|Washington           |compressed |cumulative_nb        |      25|   18.5683|               22.92|                   9.0|                -9.0|       0.36|
|Washington           |compressed |cumulative_poisson   |      25|   14.9065|               18.22|                   7.0|                -7.0|       0.40|
|Washington           |compressed |empirical_multiplier |      25|    7.0400|                7.04|                   2.0|                 0.0|       0.36|
|Washington           |compressed |hurdle_ztnb          |      25|    7.1673|                7.64|                   2.0|                -1.0|       0.80|
|Washington           |compressed |hurdle_ztpoisson     |      25|   14.9896|               20.60|                   2.0|                -1.0|       0.80|
|West Virginia        |calendar   |cumulative_nb        |      25|    3.0410|                3.68|                   0.0|                 0.0|       0.88|
|West Virginia        |calendar   |cumulative_poisson   |      25|    3.3988|                4.30|                   1.0|                 0.0|       0.84|
|West Virginia        |calendar   |empirical_multiplier |      25|    0.9200|                0.92|                   0.0|                 0.0|       0.84|
|West Virginia        |calendar   |hurdle_ztnb          |      25|    0.9380|                0.92|                   0.0|                 0.0|       0.92|
|West Virginia        |calendar   |hurdle_ztpoisson     |      25|    0.7842|                0.92|                   0.0|                 0.0|       0.92|
|West Virginia        |compressed |cumulative_nb        |      25|    1.6393|                1.84|                   0.0|                 0.0|       0.92|
|West Virginia        |compressed |cumulative_poisson   |      25|    1.5819|                1.80|                   0.0|                 0.0|       0.92|
|West Virginia        |compressed |empirical_multiplier |      25|    0.9200|                0.92|                   0.0|                 0.0|       0.84|
|West Virginia        |compressed |hurdle_ztnb          |      25|    0.8929|                0.92|                   0.0|                 0.0|       0.88|
|West Virginia        |compressed |hurdle_ztpoisson     |      25|    0.8839|                0.84|                   0.0|                 0.0|       0.88|
|Wisconsin            |calendar   |cumulative_nb        |      25|    6.8890|                8.44|                   1.0|                 0.0|       0.88|
|Wisconsin            |calendar   |cumulative_poisson   |      25|    9.5059|               11.08|                   1.0|                 0.0|       0.76|
|Wisconsin            |calendar   |empirical_multiplier |      25|   10.4000|               10.40|                   0.0|                 0.0|       0.52|
|Wisconsin            |calendar   |hurdle_ztnb          |      25|    9.0960|               10.18|                   1.0|                 0.0|       0.84|
|Wisconsin            |calendar   |hurdle_ztpoisson     |      25|    8.5637|               10.26|                   1.0|                 0.0|       0.68|
|Wisconsin            |compressed |cumulative_nb        |      25|    8.7743|                9.92|                   1.0|                 0.0|       0.80|
|Wisconsin            |compressed |cumulative_poisson   |      25|    8.7379|                9.82|                   0.0|                 0.0|       0.80|
|Wisconsin            |compressed |empirical_multiplier |      25|   10.4000|               10.40|                   0.0|                 0.0|       0.52|
|Wisconsin            |compressed |hurdle_ztnb          |      25|    9.9774|               10.14|                   0.0|                 0.0|       0.76|
|Wisconsin            |compressed |hurdle_ztpoisson     |      25|   11.2979|               13.76|                   4.0|                -4.0|       0.60|
|Wyoming              |calendar   |cumulative_nb        |      25|    2.4649|                3.10|                   2.0|                 0.0|       0.76|
|Wyoming              |calendar   |cumulative_poisson   |      25|    2.4800|                3.14|                   2.0|                 0.0|       0.72|
|Wyoming              |calendar   |empirical_multiplier |      25|    2.5200|                2.52|                   1.0|                 0.0|       0.44|
|Wyoming              |calendar   |hurdle_ztnb          |      25|    2.1697|                2.52|                   1.0|                 0.0|       0.72|
|Wyoming              |calendar   |hurdle_ztpoisson     |      25|    2.2543|                2.56|                   1.0|                 0.0|       0.68|
|Wyoming              |compressed |cumulative_nb        |      25|    2.2363|                2.46|                   0.0|                 0.0|       0.72|
|Wyoming              |compressed |cumulative_poisson   |      25|    2.3165|                2.46|                   0.0|                 0.0|       0.64|
|Wyoming              |compressed |empirical_multiplier |      25|    2.4400|                2.44|                   1.0|                 0.0|       0.48|
|Wyoming              |compressed |hurdle_ztnb          |      25|    2.3203|                2.44|                   1.0|                 0.0|       0.68|
|Wyoming              |compressed |hurdle_ztpoisson     |      25|    2.3264|                2.44|                   1.0|                 0.0|       0.60|

## Facet-grid plots

- `state_plots/state_facets_calendar_cumulative_nb.pdf`
- `state_plots/state_facets_calendar_cumulative_poisson.pdf`
- `state_plots/state_facets_calendar_hurdle_ztnb.pdf`
- `state_plots/state_facets_calendar_hurdle_ztpoisson.pdf`
- `state_plots/state_facets_compressed_cumulative_nb.pdf`
- `state_plots/state_facets_compressed_cumulative_poisson.pdf`
- `state_plots/state_facets_compressed_hurdle_ztnb.pdf`
- `state_plots/state_facets_compressed_hurdle_ztpoisson.pdf`
