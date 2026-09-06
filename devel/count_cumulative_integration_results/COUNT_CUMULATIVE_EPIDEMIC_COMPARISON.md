# Count-cumulative epidemic-process comparison

Calendar-time comparison of AR(1), HSGP and SIR epidemic processes for
the hurdle-ZTNB and hurdle-ZTPoisson observation laws. All other settings
are held fixed (`H = 26`, five origins, 53 locations, 250 draws).

Intervals were extracted through `tbl.now::tidy()`.
Target counts are reported for every row so incomplete groups are visible.
The comparison contains 1 failed fit(s); their targets are omitted rather than imputed.

## Failed fits



|location |origin     |clock    |epidemic |model            |error                                               |warnings                                                                                                                                                                                                                                                                          |
|:--------|:----------|:--------|:--------|:----------------|:---------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|Ohio     |2025-04-26 |calendar |sir      |hurdle_ztpoisson |Joint fit failed to converge for all init attempts. |NA/NaN function evaluation &#124; The `hurdle_ztpoisson` optimizer failed for every initialization.
✖ Last optimizer error: NA/NaN gradient evaluation
ℹ Try `count_cumulative_process(observation = "hurdle_ztnb")`; the ZTNB
  magnitude law was stable in the package-wide sweep. |

## Aggregate scores



|epidemic |model            | targets| mean_wis| mean_absolute_error| median_absolute_error| median_signed_error| coverage90|
|:--------|:----------------|-------:|--------:|-------------------:|---------------------:|-------------------:|----------:|
|ar1      |hurdle_ztnb      |    1325|  30.7148|             36.8226|                     2|                   0|     0.7970|
|ar1      |hurdle_ztpoisson |    1325|  36.9467|             39.4215|                     2|                  -1|     0.6506|
|hsgp     |hurdle_ztnb      |    1325|  25.1815|             35.6706|                     2|                   0|     0.8355|
|hsgp     |hurdle_ztpoisson |    1325|  34.5912|             38.6374|                     2|                  -1|     0.7509|
|sir      |hurdle_ztnb      |    1325|  35.3891|             38.0823|                     2|                  -1|     0.7208|
|sir      |hurdle_ztpoisson |    1320|  37.2880|             38.4739|                     2|                  -1|     0.5644|

## Per-state scores



|location             |epidemic |model            | targets| mean_wis| mean_absolute_error| median_absolute_error| median_signed_error| coverage90|
|:--------------------|:--------|:----------------|-------:|--------:|-------------------:|---------------------:|-------------------:|----------:|
|Alabama              |ar1      |hurdle_ztnb      |      25|   1.5779|                2.00|                   0.0|                 0.0|       0.96|
|Alabama              |ar1      |hurdle_ztpoisson |      25|   1.6993|                2.08|                   0.0|                 0.0|       0.80|
|Alabama              |hsgp     |hurdle_ztnb      |      25|   1.5971|                1.60|                   0.0|                 0.0|       0.96|
|Alabama              |hsgp     |hurdle_ztpoisson |      25|   1.8181|                1.88|                   0.0|                 0.0|       0.96|
|Alabama              |sir      |hurdle_ztnb      |      25|   1.5887|                2.00|                   0.0|                 0.0|       0.96|
|Alabama              |sir      |hurdle_ztpoisson |      25|   1.8893|                2.12|                   0.0|                 0.0|       0.76|
|Alaska               |ar1      |hurdle_ztnb      |      25|   2.4762|                3.52|                   2.0|                -2.0|       0.80|
|Alaska               |ar1      |hurdle_ztpoisson |      25|   2.2445|                3.42|                   2.0|                -2.0|       0.84|
|Alaska               |hsgp     |hurdle_ztnb      |      25|   2.1850|                3.44|                   2.0|                -2.0|       0.88|
|Alaska               |hsgp     |hurdle_ztpoisson |      25|   1.9736|                3.04|                   2.0|                -2.0|       0.96|
|Alaska               |sir      |hurdle_ztnb      |      25|   2.5972|                3.66|                   2.0|                -2.0|       0.76|
|Alaska               |sir      |hurdle_ztpoisson |      25|   2.8064|                3.56|                   2.0|                -2.0|       0.60|
|Arizona              |ar1      |hurdle_ztnb      |      25|  33.4157|               21.42|                  22.0|                -3.5|       0.92|
|Arizona              |ar1      |hurdle_ztpoisson |      25|  26.0385|               37.92|                  29.0|               -27.0|       0.72|
|Arizona              |hsgp     |hurdle_ztnb      |      25|  15.9434|               18.30|                  16.0|                -9.0|       0.84|
|Arizona              |hsgp     |hurdle_ztpoisson |      25|  27.5996|               28.26|                  25.0|               -17.0|       0.56|
|Arizona              |sir      |hurdle_ztnb      |      25|  14.6259|               19.08|                  17.0|                -9.0|       0.92|
|Arizona              |sir      |hurdle_ztpoisson |      25|  21.5405|               19.00|                  16.0|               -11.0|       0.68|
|Arkansas             |ar1      |hurdle_ztnb      |      25|  12.2435|               14.30|                   2.0|                -2.0|       0.80|
|Arkansas             |ar1      |hurdle_ztpoisson |      25|  10.9213|               13.60|                   3.0|                -1.0|       0.68|
|Arkansas             |hsgp     |hurdle_ztnb      |      25|   7.8405|               12.16|                   3.0|                -1.0|       0.88|
|Arkansas             |hsgp     |hurdle_ztpoisson |      25|   9.0935|               12.38|                   4.0|                -1.0|       0.72|
|Arkansas             |sir      |hurdle_ztnb      |      25|  12.3743|               14.52|                   2.0|                -2.0|       0.80|
|Arkansas             |sir      |hurdle_ztpoisson |      25|  11.6922|               13.90|                   2.0|                -1.0|       0.64|
|California           |ar1      |hurdle_ztnb      |      25|  89.8969|              104.46|                  21.5|               -21.5|       0.64|
|California           |ar1      |hurdle_ztpoisson |      25| 105.7386|              109.84|                  24.0|               -24.0|       0.04|
|California           |hsgp     |hurdle_ztnb      |      25|  71.3538|               95.92|                  21.0|               -21.0|       0.76|
|California           |hsgp     |hurdle_ztpoisson |      25|  97.3307|              109.08|                  24.0|               -24.0|       0.36|
|California           |sir      |hurdle_ztnb      |      25|  99.2802|              109.44|                  23.0|               -23.0|       0.52|
|California           |sir      |hurdle_ztpoisson |      25| 106.4026|              109.84|                  24.0|               -24.0|       0.00|
|Colorado             |ar1      |hurdle_ztnb      |      25|   1.7317|                1.82|                   0.5|                 0.0|       0.96|
|Colorado             |ar1      |hurdle_ztpoisson |      25|   1.7564|                2.28|                   0.0|                 0.0|       0.80|
|Colorado             |hsgp     |hurdle_ztnb      |      25|   2.2499|                2.34|                   0.0|                 0.0|       0.96|
|Colorado             |hsgp     |hurdle_ztpoisson |      25|   2.0601|                1.84|                   0.0|                 0.0|       0.88|
|Colorado             |sir      |hurdle_ztnb      |      25|   1.4457|                1.88|                   1.0|                 0.0|       0.96|
|Colorado             |sir      |hurdle_ztpoisson |      25|   1.7209|                2.24|                   0.0|                 0.0|       0.76|
|Connecticut          |ar1      |hurdle_ztnb      |      25|   2.3301|                2.52|                   0.0|                 0.0|       0.96|
|Connecticut          |ar1      |hurdle_ztpoisson |      25|   1.9869|                2.36|                   0.0|                 0.0|       0.88|
|Connecticut          |hsgp     |hurdle_ztnb      |      25|   2.5545|                3.18|                   0.0|                 0.0|       0.96|
|Connecticut          |hsgp     |hurdle_ztpoisson |      25|   2.2920|                2.40|                   0.0|                 0.0|       0.92|
|Connecticut          |sir      |hurdle_ztnb      |      25|   1.9907|                2.40|                   0.0|                 0.0|       0.92|
|Connecticut          |sir      |hurdle_ztpoisson |      25|   2.0554|                2.30|                   0.0|                 0.0|       0.88|
|Delaware             |ar1      |hurdle_ztnb      |      25|  16.8987|               17.72|                   9.0|                -9.0|       0.44|
|Delaware             |ar1      |hurdle_ztpoisson |      25|  15.1755|               17.56|                   9.0|                -9.0|       0.48|
|Delaware             |hsgp     |hurdle_ztnb      |      25|  15.0558|               17.26|                   9.0|                -9.0|       0.36|
|Delaware             |hsgp     |hurdle_ztpoisson |      25|  13.5463|               17.18|                   9.0|                -9.0|       0.44|
|Delaware             |sir      |hurdle_ztnb      |      25|  17.7200|               17.72|                   9.0|                -9.0|       0.24|
|Delaware             |sir      |hurdle_ztpoisson |      25|  17.7200|               17.72|                   9.0|                -9.0|       0.24|
|District of Columbia |ar1      |hurdle_ztnb      |      25|   3.4590|                3.86|                   1.0|                 0.0|       0.60|
|District of Columbia |ar1      |hurdle_ztpoisson |      25|   2.9555|                3.70|                   2.0|                 0.0|       0.68|
|District of Columbia |hsgp     |hurdle_ztnb      |      25|   2.7917|                3.60|                   2.0|                 0.0|       0.72|
|District of Columbia |hsgp     |hurdle_ztpoisson |      25|   2.9175|                3.64|                   1.0|                 0.0|       0.76|
|District of Columbia |sir      |hurdle_ztnb      |      25|   3.8400|                3.84|                   1.0|                 0.0|       0.48|
|District of Columbia |sir      |hurdle_ztpoisson |      25|   3.8400|                3.84|                   1.0|                 0.0|       0.48|
|Florida              |ar1      |hurdle_ztnb      |      25|  44.6011|               60.34|                  22.5|               -22.5|       0.80|
|Florida              |ar1      |hurdle_ztpoisson |      25|  62.6681|               66.48|                  28.0|               -28.0|       0.12|
|Florida              |hsgp     |hurdle_ztnb      |      25|  32.5689|               55.50|                  19.0|               -19.0|       0.92|
|Florida              |hsgp     |hurdle_ztpoisson |      25|  53.2948|               65.18|                  28.0|               -28.0|       0.56|
|Florida              |sir      |hurdle_ztnb      |      25|  54.3495|               65.84|                  26.0|               -26.0|       0.68|
|Florida              |sir      |hurdle_ztpoisson |      25|  64.2713|               66.48|                  28.0|               -28.0|       0.08|
|Georgia              |ar1      |hurdle_ztnb      |      25|  21.3970|               24.26|                   4.0|                -1.0|       0.84|
|Georgia              |ar1      |hurdle_ztpoisson |      25|  20.3207|               23.76|                   3.0|                -2.0|       0.80|
|Georgia              |hsgp     |hurdle_ztnb      |      25|  12.8385|               19.38|                   2.5|                -1.0|       0.96|
|Georgia              |hsgp     |hurdle_ztpoisson |      25|  18.2030|               23.16|                   2.0|                -2.0|       0.84|
|Georgia              |sir      |hurdle_ztnb      |      25|  21.9651|               24.22|                   2.0|                -2.0|       0.80|
|Georgia              |sir      |hurdle_ztpoisson |      25|  20.2943|               23.98|                   2.0|                -2.0|       0.76|
|Hawaii               |ar1      |hurdle_ztnb      |      25|   0.7455|                0.64|                   0.0|                 0.0|       0.96|
|Hawaii               |ar1      |hurdle_ztpoisson |      25|   1.1716|                1.08|                   0.0|                 0.0|       1.00|
|Hawaii               |hsgp     |hurdle_ztnb      |      25|   1.3184|                1.56|                   0.0|                 0.0|       1.00|
|Hawaii               |hsgp     |hurdle_ztpoisson |      25|   1.6573|                1.52|                   0.0|                 0.0|       1.00|
|Hawaii               |sir      |hurdle_ztnb      |      25|   0.7993|                0.70|                   0.0|                 0.0|       1.00|
|Hawaii               |sir      |hurdle_ztpoisson |      25|   0.7636|                0.92|                   0.0|                 0.0|       1.00|
|Idaho                |ar1      |hurdle_ztnb      |      25|   2.6081|                3.24|                   0.0|                 0.0|       0.80|
|Idaho                |ar1      |hurdle_ztpoisson |      25|   2.6000|                3.24|                   0.0|                 0.0|       0.84|
|Idaho                |hsgp     |hurdle_ztnb      |      25|   2.1442|                2.98|                   0.0|                 0.0|       0.88|
|Idaho                |hsgp     |hurdle_ztpoisson |      25|   2.3486|                3.36|                   0.0|                 0.0|       0.88|
|Idaho                |sir      |hurdle_ztnb      |      25|   3.2400|                3.24|                   0.0|                 0.0|       0.56|
|Idaho                |sir      |hurdle_ztpoisson |      25|   3.2400|                3.24|                   0.0|                 0.0|       0.56|
|Illinois             |ar1      |hurdle_ztnb      |      25|  26.6200|               29.98|                   5.0|                -2.0|       0.72|
|Illinois             |ar1      |hurdle_ztpoisson |      25|  29.0044|               30.44|                   4.5|                -3.0|       0.32|
|Illinois             |hsgp     |hurdle_ztnb      |      25|  18.2488|               26.56|                   5.0|                -1.0|       0.76|
|Illinois             |hsgp     |hurdle_ztpoisson |      25|  26.7277|               31.14|                   5.0|                -3.0|       0.76|
|Illinois             |sir      |hurdle_ztnb      |      25|  26.9639|               30.78|                   3.0|                -3.0|       0.72|
|Illinois             |sir      |hurdle_ztpoisson |      25|  29.3848|               30.80|                   4.0|                -4.0|       0.36|
|Indiana              |ar1      |hurdle_ztnb      |      25|  18.1596|               19.84|                   1.0|                 0.0|       0.80|
|Indiana              |ar1      |hurdle_ztpoisson |      25|  26.6317|               34.64|                   1.0|                -1.0|       0.68|
|Indiana              |hsgp     |hurdle_ztnb      |      25|  16.3479|               21.50|                   1.0|                -1.0|       0.88|
|Indiana              |hsgp     |hurdle_ztpoisson |      25|  15.0449|               21.70|                   4.0|                -0.5|       0.88|
|Indiana              |sir      |hurdle_ztnb      |      25|  17.1068|               19.76|                   1.0|                -1.0|       0.76|
|Indiana              |sir      |hurdle_ztpoisson |      25|  22.7443|               26.04|                   1.0|                -1.0|       0.60|
|Iowa                 |ar1      |hurdle_ztnb      |      25|   6.3520|                7.28|                   2.0|                 0.0|       0.80|
|Iowa                 |ar1      |hurdle_ztpoisson |      25|   4.1625|                5.98|                   1.0|                -1.0|       0.88|
|Iowa                 |hsgp     |hurdle_ztnb      |      25|   2.7269|                4.44|                   1.0|                 0.0|       0.92|
|Iowa                 |hsgp     |hurdle_ztpoisson |      25|   2.8243|                4.58|                   0.0|                 0.0|       0.96|
|Iowa                 |sir      |hurdle_ztnb      |      25|   6.0420|                7.24|                   2.0|                 0.0|       0.80|
|Iowa                 |sir      |hurdle_ztpoisson |      25|   5.3324|                6.60|                   1.0|                -1.0|       0.72|
|Kansas               |ar1      |hurdle_ztnb      |      25|   5.1677|                6.22|                   2.0|                 0.0|       0.88|
|Kansas               |ar1      |hurdle_ztpoisson |      25|   3.7912|                5.36|                   1.0|                -1.0|       0.88|
|Kansas               |hsgp     |hurdle_ztnb      |      25|   2.9145|                4.66|                   1.0|                 0.0|       0.96|
|Kansas               |hsgp     |hurdle_ztpoisson |      25|   3.9274|                5.38|                   1.0|                 0.0|       0.92|
|Kansas               |sir      |hurdle_ztnb      |      25|   3.8603|                5.42|                   1.0|                -1.0|       0.92|
|Kansas               |sir      |hurdle_ztpoisson |      25|   4.9352|                5.94|                   1.0|                -1.0|       0.72|
|Kentucky             |ar1      |hurdle_ztnb      |      25|  17.2175|               20.28|                   7.0|                -7.0|       0.68|
|Kentucky             |ar1      |hurdle_ztpoisson |      25|  16.7430|               20.10|                   7.0|                -7.0|       0.56|
|Kentucky             |hsgp     |hurdle_ztnb      |      25|  10.9447|               16.36|                   6.0|                -6.0|       0.72|
|Kentucky             |hsgp     |hurdle_ztpoisson |      25|  13.3326|               18.08|                   6.0|                -6.0|       0.72|
|Kentucky             |sir      |hurdle_ztnb      |      25|  17.7852|               20.64|                   8.0|                -8.0|       0.68|
|Kentucky             |sir      |hurdle_ztpoisson |      25|  18.9321|               20.42|                   7.5|                -7.5|       0.36|
|Louisiana            |ar1      |hurdle_ztnb      |      25|  16.2224|               17.58|                  10.0|                 2.0|       0.48|
|Louisiana            |ar1      |hurdle_ztpoisson |      25|  16.0514|               17.00|                   9.0|                 3.0|       0.32|
|Louisiana            |hsgp     |hurdle_ztnb      |      25|  12.7725|               15.78|                   9.0|                 0.0|       0.56|
|Louisiana            |hsgp     |hurdle_ztpoisson |      25|  15.7539|               16.62|                   8.0|                 0.0|       0.48|
|Louisiana            |sir      |hurdle_ztnb      |      25|  15.7387|               17.08|                   9.0|                 0.0|       0.44|
|Louisiana            |sir      |hurdle_ztpoisson |      25|  16.0413|               17.06|                   9.0|                 1.5|       0.32|
|Maine                |ar1      |hurdle_ztnb      |      25|   1.3198|                1.52|                   0.0|                 0.0|       0.80|
|Maine                |ar1      |hurdle_ztpoisson |      25|   1.1740|                1.30|                   0.0|                 0.0|       0.84|
|Maine                |hsgp     |hurdle_ztnb      |      25|   1.1117|                1.20|                   0.0|                 0.0|       0.84|
|Maine                |hsgp     |hurdle_ztpoisson |      25|   1.3420|                1.60|                   1.0|                 0.0|       0.84|
|Maine                |sir      |hurdle_ztnb      |      25|   1.4800|                1.48|                   0.0|                 0.0|       0.68|
|Maine                |sir      |hurdle_ztpoisson |      25|   1.4800|                1.48|                   0.0|                 0.0|       0.68|
|Maryland             |ar1      |hurdle_ztnb      |      25|  16.3725|               18.30|                   1.0|                -1.0|       0.72|
|Maryland             |ar1      |hurdle_ztpoisson |      25|  16.4056|               19.40|                   3.0|                -1.0|       0.60|
|Maryland             |hsgp     |hurdle_ztnb      |      25|  11.5182|               16.30|                   1.0|                -1.0|       0.88|
|Maryland             |hsgp     |hurdle_ztpoisson |      25|  12.3674|               17.30|                   2.0|                -1.0|       0.80|
|Maryland             |sir      |hurdle_ztnb      |      25|  16.6393|               18.40|                   1.0|                -1.0|       0.76|
|Maryland             |sir      |hurdle_ztpoisson |      25|  17.4481|               18.28|                   1.0|                -1.0|       0.48|
|Massachusetts        |ar1      |hurdle_ztnb      |      25|  36.9863|               37.92|                   9.0|                -9.0|       0.44|
|Massachusetts        |ar1      |hurdle_ztpoisson |      25|  45.8987|               53.44|                  15.0|               -15.0|       0.24|
|Massachusetts        |hsgp     |hurdle_ztnb      |      25|  34.2072|               38.92|                   9.0|                -9.0|       0.48|
|Massachusetts        |hsgp     |hurdle_ztpoisson |      25|  39.4315|               49.14|                  12.0|               -12.0|       0.32|
|Massachusetts        |sir      |hurdle_ztnb      |      25|  37.5154|               37.92|                   9.0|                -9.0|       0.36|
|Massachusetts        |sir      |hurdle_ztpoisson |      25|  42.0510|               44.08|                   9.0|                -9.0|       0.20|
|Michigan             |ar1      |hurdle_ztnb      |      25| 106.2160|              116.44|                  59.0|               -59.0|       0.52|
|Michigan             |ar1      |hurdle_ztpoisson |      25| 112.2454|              116.64|                  59.0|               -59.0|       0.12|
|Michigan             |hsgp     |hurdle_ztnb      |      25| 103.0274|              116.52|                  59.0|               -59.0|       0.32|
|Michigan             |hsgp     |hurdle_ztpoisson |      25| 103.1481|              115.08|                  59.0|               -59.0|       0.20|
|Michigan             |sir      |hurdle_ztnb      |      25| 112.6523|              116.64|                  59.0|               -59.0|       0.32|
|Michigan             |sir      |hurdle_ztpoisson |      25| 113.3751|              116.64|                  59.0|               -59.0|       0.08|
|Minnesota            |ar1      |hurdle_ztnb      |      25|  16.1283|               15.08|                   0.0|                 0.0|       0.72|
|Minnesota            |ar1      |hurdle_ztpoisson |      25|  12.7024|               15.08|                   0.0|                 0.0|       0.80|
|Minnesota            |hsgp     |hurdle_ztnb      |      25|  14.7621|               15.04|                   0.0|                 0.0|       0.76|
|Minnesota            |hsgp     |hurdle_ztpoisson |      25|  14.3734|               15.08|                   0.0|                 0.0|       0.64|
|Minnesota            |sir      |hurdle_ztnb      |      25|  15.0800|               15.08|                   0.0|                 0.0|       0.52|
|Minnesota            |sir      |hurdle_ztpoisson |      25|  15.0800|               15.08|                   0.0|                 0.0|       0.52|
|Mississippi          |ar1      |hurdle_ztnb      |      25|   2.9765|                3.16|                   1.0|                 0.0|       0.92|
|Mississippi          |ar1      |hurdle_ztpoisson |      25|   2.9384|                3.44|                   1.0|                 0.0|       0.92|
|Mississippi          |hsgp     |hurdle_ztnb      |      25|   3.0857|                3.52|                   1.0|                 0.0|       0.92|
|Mississippi          |hsgp     |hurdle_ztpoisson |      25|   2.9206|                3.78|                   1.0|                 0.0|       0.92|
|Mississippi          |sir      |hurdle_ztnb      |      25|   2.7679|                3.24|                   1.0|                 0.0|       0.92|
|Mississippi          |sir      |hurdle_ztpoisson |      25|   2.9533|                3.32|                   0.0|                 0.0|       0.88|
|Missouri             |ar1      |hurdle_ztnb      |      25|  30.3549|               34.26|                   3.0|                 0.0|       0.88|
|Missouri             |ar1      |hurdle_ztpoisson |      25|  30.3818|               34.36|                   4.0|                 0.0|       0.76|
|Missouri             |hsgp     |hurdle_ztnb      |      25|  24.1683|               32.92|                   8.0|                 1.0|       0.92|
|Missouri             |hsgp     |hurdle_ztpoisson |      25|  25.9006|               33.14|                   4.0|                 0.0|       0.76|
|Missouri             |sir      |hurdle_ztnb      |      25|  31.1293|               35.22|                   4.0|                 0.0|       0.88|
|Missouri             |sir      |hurdle_ztpoisson |      25|  31.2793|               34.82|                   3.0|                 0.0|       0.72|
|Montana              |ar1      |hurdle_ztnb      |      25|   3.7762|                4.24|                   3.0|                 0.0|       0.48|
|Montana              |ar1      |hurdle_ztpoisson |      25|   3.4393|                4.60|                   4.0|                 0.0|       0.52|
|Montana              |hsgp     |hurdle_ztnb      |      25|   3.4676|                4.58|                   4.0|                 0.0|       0.60|
|Montana              |hsgp     |hurdle_ztpoisson |      25|   3.3364|                4.50|                   3.0|                 0.0|       0.64|
|Montana              |sir      |hurdle_ztnb      |      25|   4.2400|                4.24|                   3.0|                 0.0|       0.28|
|Montana              |sir      |hurdle_ztpoisson |      25|   4.2400|                4.24|                   3.0|                 0.0|       0.28|
|Nebraska             |ar1      |hurdle_ztnb      |      25|   1.5438|                1.84|                   0.0|                 0.0|       0.88|
|Nebraska             |ar1      |hurdle_ztpoisson |      25|   1.0614|                1.32|                   0.0|                 0.0|       0.92|
|Nebraska             |hsgp     |hurdle_ztnb      |      25|   0.9488|                1.40|                   0.0|                 0.0|       0.96|
|Nebraska             |hsgp     |hurdle_ztpoisson |      25|   1.1321|                1.12|                   0.0|                 0.0|       0.96|
|Nebraska             |sir      |hurdle_ztnb      |      25|   1.1786|                1.62|                   0.0|                 0.0|       0.96|
|Nebraska             |sir      |hurdle_ztpoisson |      25|   1.2498|                1.64|                   0.0|                 0.0|       0.96|
|Nevada               |ar1      |hurdle_ztnb      |      25|   3.4091|                3.96|                   1.0|                 0.0|       0.84|
|Nevada               |ar1      |hurdle_ztpoisson |      25|   2.7933|                3.80|                   0.0|                 0.0|       0.84|
|Nevada               |hsgp     |hurdle_ztnb      |      25|   2.4952|                3.08|                   0.0|                 0.0|       0.92|
|Nevada               |hsgp     |hurdle_ztpoisson |      25|   2.4840|                3.46|                   1.5|                 0.0|       0.92|
|Nevada               |sir      |hurdle_ztnb      |      25|   2.8518|                3.76|                   0.5|                 0.0|       0.92|
|Nevada               |sir      |hurdle_ztpoisson |      25|   3.2418|                4.04|                   0.0|                 0.0|       0.68|
|New Hampshire        |ar1      |hurdle_ztnb      |      25|  12.4866|               12.68|                   0.0|                 0.0|       0.68|
|New Hampshire        |ar1      |hurdle_ztpoisson |      25|  11.6686|               12.64|                   0.0|                 0.0|       0.72|
|New Hampshire        |hsgp     |hurdle_ztnb      |      25|  11.6200|               12.92|                   0.0|                 0.0|       0.68|
|New Hampshire        |hsgp     |hurdle_ztpoisson |      25|  12.6495|               13.00|                   0.0|                 0.0|       0.68|
|New Hampshire        |sir      |hurdle_ztnb      |      25|  12.1324|               12.64|                   0.0|                 0.0|       0.72|
|New Hampshire        |sir      |hurdle_ztpoisson |      25|  12.1619|               12.64|                   0.0|                 0.0|       0.68|
|New Jersey           |ar1      |hurdle_ztnb      |      25|   2.7913|                1.90|                   0.0|                 0.0|       1.00|
|New Jersey           |ar1      |hurdle_ztpoisson |      25|   2.2518|                1.84|                   0.0|                 0.0|       0.72|
|New Jersey           |hsgp     |hurdle_ztnb      |      25|   5.4156|                6.06|                   1.0|                 0.0|       1.00|
|New Jersey           |hsgp     |hurdle_ztpoisson |      25|   4.0379|                2.04|                   0.0|                 0.0|       0.96|
|New Jersey           |sir      |hurdle_ztnb      |      25|   1.8524|                1.84|                   0.0|                 0.0|       0.96|
|New Jersey           |sir      |hurdle_ztpoisson |      25|   2.0444|                1.84|                   0.0|                 0.0|       0.72|
|New Mexico           |ar1      |hurdle_ztnb      |      25|   7.0893|                8.96|                   2.0|                -1.0|       0.92|
|New Mexico           |ar1      |hurdle_ztpoisson |      25|   7.8293|                8.88|                   2.0|                -1.0|       0.80|
|New Mexico           |hsgp     |hurdle_ztnb      |      25|   5.8013|                7.82|                   1.5|                 0.0|       0.92|
|New Mexico           |hsgp     |hurdle_ztpoisson |      25|   7.2180|                8.68|                   1.0|                -1.0|       0.92|
|New Mexico           |sir      |hurdle_ztnb      |      25|   6.8747|                8.60|                   1.0|                -1.0|       0.88|
|New Mexico           |sir      |hurdle_ztpoisson |      25|   8.1176|                9.08|                   2.0|                -1.0|       0.68|
|New York             |ar1      |hurdle_ztnb      |      25|  21.8359|               27.90|                   7.0|                -1.0|       0.84|
|New York             |ar1      |hurdle_ztpoisson |      25|  27.3913|               29.40|                   9.0|                -8.0|       0.48|
|New York             |hsgp     |hurdle_ztnb      |      25|  12.6079|               21.52|                   4.0|                -4.0|       1.00|
|New York             |hsgp     |hurdle_ztpoisson |      25|  23.7294|               29.36|                   8.0|                -8.0|       0.48|
|New York             |sir      |hurdle_ztnb      |      25|  24.2017|               28.82|                   7.0|                -7.0|       0.72|
|New York             |sir      |hurdle_ztpoisson |      25|  28.5147|               29.48|                   8.0|                -8.0|       0.36|
|North Carolina       |ar1      |hurdle_ztnb      |      25|  18.2688|               22.08|                   2.0|                -1.0|       0.84|
|North Carolina       |ar1      |hurdle_ztpoisson |      25|  21.3864|               22.12|                   1.0|                -1.0|       0.48|
|North Carolina       |hsgp     |hurdle_ztnb      |      25|  14.3769|               21.42|                   1.0|                -1.0|       0.96|
|North Carolina       |hsgp     |hurdle_ztpoisson |      25|  17.8255|               22.12|                   1.0|                -1.0|       0.76|
|North Carolina       |sir      |hurdle_ztnb      |      25|  19.9738|               22.12|                   1.0|                -1.0|       0.80|
|North Carolina       |sir      |hurdle_ztpoisson |      25|  21.9514|               22.12|                   1.0|                -1.0|       0.44|
|North Dakota         |ar1      |hurdle_ztnb      |      25|   0.6004|                0.76|                   0.0|                 0.0|       0.96|
|North Dakota         |ar1      |hurdle_ztpoisson |      25|   0.7071|                0.72|                   0.0|                 0.0|       0.96|
|North Dakota         |hsgp     |hurdle_ztnb      |      25|   0.6696|                0.72|                   0.0|                 0.0|       0.96|
|North Dakota         |hsgp     |hurdle_ztpoisson |      25|   0.7802|                0.96|                   0.0|                 0.0|       0.96|
|North Dakota         |sir      |hurdle_ztnb      |      25|   0.7600|                0.76|                   0.0|                 0.0|       0.80|
|North Dakota         |sir      |hurdle_ztpoisson |      25|   0.7600|                0.76|                   0.0|                 0.0|       0.80|
|Ohio                 |ar1      |hurdle_ztnb      |      25|  26.7008|               33.30|                   6.0|                 1.0|       0.72|
|Ohio                 |ar1      |hurdle_ztpoisson |      25|  22.7263|               31.86|                   6.0|                -0.5|       0.72|
|Ohio                 |hsgp     |hurdle_ztnb      |      25|  18.2476|               29.24|                   5.5|                 0.0|       0.80|
|Ohio                 |hsgp     |hurdle_ztpoisson |      25|  19.8987|               32.36|                   5.0|                 0.0|       0.88|
|Ohio                 |sir      |hurdle_ztnb      |      25|  28.8330|               32.94|                   5.0|                 0.0|       0.60|
|Ohio                 |sir      |hurdle_ztpoisson |      20|  32.7715|               41.00|                   9.5|                -1.5|       0.55|
|Oklahoma             |ar1      |hurdle_ztnb      |      25|  18.0847|               19.76|                   1.0|                 0.0|       0.84|
|Oklahoma             |ar1      |hurdle_ztpoisson |      25|  17.0431|               20.24|                   2.0|                 0.0|       0.64|
|Oklahoma             |hsgp     |hurdle_ztnb      |      25|  12.4025|               18.80|                   4.0|                 0.0|       0.88|
|Oklahoma             |hsgp     |hurdle_ztpoisson |      25|  14.9903|               19.90|                   4.0|                 0.0|       0.76|
|Oklahoma             |sir      |hurdle_ztnb      |      25|  18.0119|               19.96|                   1.0|                 0.0|       0.84|
|Oklahoma             |sir      |hurdle_ztpoisson |      25|  18.4242|               20.40|                   2.0|                 0.0|       0.60|
|Oregon               |ar1      |hurdle_ztnb      |      25|   7.6120|                9.88|                   2.0|                -2.0|       0.84|
|Oregon               |ar1      |hurdle_ztpoisson |      25|   7.6671|                9.68|                   3.0|                -2.0|       0.68|
|Oregon               |hsgp     |hurdle_ztnb      |      25|   5.8194|                8.42|                   2.0|                -1.0|       0.84|
|Oregon               |hsgp     |hurdle_ztpoisson |      25|   7.3075|                8.68|                   2.0|                -2.0|       0.84|
|Oregon               |sir      |hurdle_ztnb      |      25|   7.2524|               10.28|                   2.0|                -2.0|       0.80|
|Oregon               |sir      |hurdle_ztpoisson |      25|   8.0106|               10.24|                   3.0|                -2.0|       0.72|
|Pennsylvania         |ar1      |hurdle_ztnb      |      25|  62.5615|               87.10|                  68.0|               -68.0|       0.80|
|Pennsylvania         |ar1      |hurdle_ztpoisson |      25|  86.6454|               88.92|                  70.0|               -70.0|       0.08|
|Pennsylvania         |hsgp     |hurdle_ztnb      |      25|  62.8368|               84.26|                  70.0|               -70.0|       0.56|
|Pennsylvania         |hsgp     |hurdle_ztpoisson |      25|  78.1245|               88.28|                  66.0|               -66.0|       0.28|
|Pennsylvania         |sir      |hurdle_ztnb      |      25|  77.5101|               88.92|                  70.0|               -70.0|       0.36|
|Pennsylvania         |sir      |hurdle_ztpoisson |      25|  87.8945|               88.92|                  70.0|               -70.0|       0.08|
|Puerto Rico          |ar1      |hurdle_ztnb      |      25|  93.1469|              100.84|                  24.0|               -22.0|       0.68|
|Puerto Rico          |ar1      |hurdle_ztpoisson |      25| 106.2246|              120.14|                  66.5|               -66.5|       0.48|
|Puerto Rico          |hsgp     |hurdle_ztnb      |      25|  94.9499|              101.90|                  26.0|               -26.0|       0.48|
|Puerto Rico          |hsgp     |hurdle_ztpoisson |      25| 106.8624|              121.02|                  64.0|               -64.0|       0.52|
|Puerto Rico          |sir      |hurdle_ztnb      |      25|  92.3820|              102.06|                  26.0|               -26.0|       0.68|
|Puerto Rico          |sir      |hurdle_ztpoisson |      25|  97.0147|              101.88|                  26.0|               -26.0|       0.32|
|Rhode Island         |ar1      |hurdle_ztnb      |      25|   3.6319|                4.04|                   0.0|                 0.0|       0.72|
|Rhode Island         |ar1      |hurdle_ztpoisson |      25|   3.2296|                3.72|                   0.0|                 0.0|       0.76|
|Rhode Island         |hsgp     |hurdle_ztnb      |      25|   3.0861|                3.76|                   0.0|                 0.0|       0.80|
|Rhode Island         |hsgp     |hurdle_ztpoisson |      25|   3.1880|                3.92|                   0.0|                 0.0|       0.80|
|Rhode Island         |sir      |hurdle_ztnb      |      25|   4.0400|                4.04|                   0.0|                 0.0|       0.64|
|Rhode Island         |sir      |hurdle_ztpoisson |      25|   4.0400|                4.04|                   0.0|                 0.0|       0.64|
|South Carolina       |ar1      |hurdle_ztnb      |      25|   6.3121|                7.72|                   3.0|                -2.0|       0.92|
|South Carolina       |ar1      |hurdle_ztpoisson |      25|   6.8257|                7.92|                   3.0|                -3.0|       0.56|
|South Carolina       |hsgp     |hurdle_ztnb      |      25|   4.7896|                7.48|                   3.0|                -2.0|       0.92|
|South Carolina       |hsgp     |hurdle_ztpoisson |      25|   5.1472|                7.08|                   2.0|                -2.0|       0.84|
|South Carolina       |sir      |hurdle_ztnb      |      25|   6.0964|                7.92|                   3.0|                -3.0|       0.88|
|South Carolina       |sir      |hurdle_ztpoisson |      25|   7.1233|                7.92|                   3.0|                -3.0|       0.48|
|South Dakota         |ar1      |hurdle_ztnb      |      25|   1.5752|                1.80|                   0.0|                 0.0|       0.80|
|South Dakota         |ar1      |hurdle_ztpoisson |      25|   1.0260|                1.36|                   0.0|                 0.0|       0.88|
|South Dakota         |hsgp     |hurdle_ztnb      |      25|   0.8992|                1.36|                   0.0|                 0.0|       0.96|
|South Dakota         |hsgp     |hurdle_ztpoisson |      25|   1.0077|                1.40|                   0.0|                 0.0|       0.88|
|South Dakota         |sir      |hurdle_ztnb      |      25|   1.4695|                1.80|                   0.0|                 0.0|       0.80|
|South Dakota         |sir      |hurdle_ztpoisson |      25|   1.0407|                1.48|                   0.0|                 0.0|       0.88|
|Tennessee            |ar1      |hurdle_ztnb      |      25|   5.5126|                7.00|                   2.0|                 0.0|       0.92|
|Tennessee            |ar1      |hurdle_ztpoisson |      25|   4.9398|                6.34|                   0.0|                 0.0|       0.84|
|Tennessee            |hsgp     |hurdle_ztnb      |      25|   4.3091|                6.12|                   1.0|                 0.0|       1.00|
|Tennessee            |hsgp     |hurdle_ztpoisson |      25|   5.4453|                6.50|                   1.0|                 0.0|       0.96|
|Tennessee            |sir      |hurdle_ztnb      |      25|   5.0930|                6.80|                   1.0|                 0.0|       0.92|
|Tennessee            |sir      |hurdle_ztpoisson |      25|   5.1881|                6.56|                   0.0|                 0.0|       0.84|
|Texas                |ar1      |hurdle_ztnb      |      25|  24.2835|               31.02|                   5.5|                -5.5|       0.92|
|Texas                |ar1      |hurdle_ztpoisson |      25|  30.9124|               34.14|                   9.0|                -9.0|       0.32|
|Texas                |hsgp     |hurdle_ztnb      |      25|  19.7954|               32.42|                   9.0|                -9.0|       0.96|
|Texas                |hsgp     |hurdle_ztpoisson |      25|  27.3945|               34.60|                   9.0|                -9.0|       0.60|
|Texas                |sir      |hurdle_ztnb      |      25|  29.2785|               34.38|                   9.0|                -9.0|       0.72|
|Texas                |sir      |hurdle_ztpoisson |      25|  32.4325|               34.60|                   9.0|                -9.0|       0.28|
|US                   |ar1      |hurdle_ztnb      |      25| 732.8288|              929.72|                 403.5|              -403.5|       0.72|
|US                   |ar1      |hurdle_ztpoisson |      25| 978.8734|              987.94|                 463.0|              -463.0|       0.00|
|US                   |hsgp     |hurdle_ztnb      |      25| 594.6890|              924.00|                 455.0|              -455.0|       0.72|
|US                   |hsgp     |hurdle_ztpoisson |      25| 942.0291|              986.98|                 456.0|              -456.0|       0.00|
|US                   |sir      |hurdle_ztnb      |      25| 950.5931|              979.38|                 456.0|              -456.0|       0.00|
|US                   |sir      |hurdle_ztpoisson |      25| 978.0026|              978.72|                 456.0|              -456.0|       0.00|
|Utah                 |ar1      |hurdle_ztnb      |      25|   3.5439|                3.56|                   0.0|                 0.0|       0.88|
|Utah                 |ar1      |hurdle_ztpoisson |      25|   2.5581|                3.30|                   0.0|                 0.0|       0.92|
|Utah                 |hsgp     |hurdle_ztnb      |      25|   2.7442|                3.42|                   0.0|                 0.0|       0.92|
|Utah                 |hsgp     |hurdle_ztpoisson |      25|   2.7996|                3.22|                   0.0|                 0.0|       0.96|
|Utah                 |sir      |hurdle_ztnb      |      25|   2.9128|                3.56|                   0.0|                 0.0|       0.92|
|Utah                 |sir      |hurdle_ztpoisson |      25|   2.7619|                3.56|                   0.0|                 0.0|       0.84|
|Vermont              |ar1      |hurdle_ztnb      |      25|   1.3175|                1.40|                   0.0|                 0.0|       0.84|
|Vermont              |ar1      |hurdle_ztpoisson |      25|   0.7206|                1.12|                   0.0|                 0.0|       0.96|
|Vermont              |hsgp     |hurdle_ztnb      |      25|   0.7515|                1.14|                   0.0|                 0.0|       1.00|
|Vermont              |hsgp     |hurdle_ztpoisson |      25|   0.7242|                1.08|                   0.0|                 0.0|       1.00|
|Vermont              |sir      |hurdle_ztnb      |      25|   1.4000|                1.40|                   0.0|                 0.0|       0.72|
|Vermont              |sir      |hurdle_ztpoisson |      25|   1.4000|                1.40|                   0.0|                 0.0|       0.72|
|Virginia             |ar1      |hurdle_ztnb      |      25|  17.5874|               21.66|                   7.5|                -5.0|       0.88|
|Virginia             |ar1      |hurdle_ztpoisson |      25|  19.2582|               22.10|                   8.0|                -8.0|       0.60|
|Virginia             |hsgp     |hurdle_ztnb      |      25|  13.1824|               20.58|                   8.0|                -8.0|       0.76|
|Virginia             |hsgp     |hurdle_ztpoisson |      25|  17.3732|               21.62|                   8.0|                -8.0|       0.52|
|Virginia             |sir      |hurdle_ztnb      |      25|  18.4262|               22.36|                   8.0|                -8.0|       0.76|
|Virginia             |sir      |hurdle_ztpoisson |      25|  19.6134|               22.44|                   8.0|                -8.0|       0.48|
|Washington           |ar1      |hurdle_ztnb      |      25|   5.7047|                6.90|                   2.0|                 0.0|       1.00|
|Washington           |ar1      |hurdle_ztpoisson |      25|   5.9868|                7.00|                   2.0|                 0.0|       0.72|
|Washington           |hsgp     |hurdle_ztnb      |      25|   4.6240|                6.34|                   2.0|                 0.0|       0.96|
|Washington           |hsgp     |hurdle_ztpoisson |      25|   5.9007|                7.00|                   2.0|                 0.0|       0.96|
|Washington           |sir      |hurdle_ztnb      |      25|   5.1888|                6.92|                   2.0|                 0.0|       1.00|
|Washington           |sir      |hurdle_ztpoisson |      25|   6.1749|                7.00|                   2.0|                 0.0|       0.64|
|West Virginia        |ar1      |hurdle_ztnb      |      25|   0.9380|                0.92|                   0.0|                 0.0|       0.92|
|West Virginia        |ar1      |hurdle_ztpoisson |      25|   0.7842|                0.92|                   0.0|                 0.0|       0.92|
|West Virginia        |hsgp     |hurdle_ztnb      |      25|   0.8661|                0.86|                   0.0|                 0.0|       1.00|
|West Virginia        |hsgp     |hurdle_ztpoisson |      25|   0.9846|                0.92|                   0.0|                 0.0|       1.00|
|West Virginia        |sir      |hurdle_ztnb      |      25|   0.8618|                0.92|                   0.0|                 0.0|       0.92|
|West Virginia        |sir      |hurdle_ztpoisson |      25|   0.7881|                0.92|                   0.0|                 0.0|       0.92|
|Wisconsin            |ar1      |hurdle_ztnb      |      25|   9.0960|               10.18|                   1.0|                 0.0|       0.84|
|Wisconsin            |ar1      |hurdle_ztpoisson |      25|   8.5637|               10.26|                   1.0|                 0.0|       0.68|
|Wisconsin            |hsgp     |hurdle_ztnb      |      25|   4.8277|                7.54|                   1.0|                 0.0|       1.00|
|Wisconsin            |hsgp     |hurdle_ztpoisson |      25|   7.5890|                9.96|                   3.0|                 0.0|       0.88|
|Wisconsin            |sir      |hurdle_ztnb      |      25|   9.1075|               10.36|                   0.0|                 0.0|       0.80|
|Wisconsin            |sir      |hurdle_ztpoisson |      25|   8.6029|               10.48|                   1.0|                 0.0|       0.80|
|Wyoming              |ar1      |hurdle_ztnb      |      25|   2.1697|                2.52|                   1.0|                 0.0|       0.72|
|Wyoming              |ar1      |hurdle_ztpoisson |      25|   2.2543|                2.56|                   1.0|                 0.0|       0.68|
|Wyoming              |hsgp     |hurdle_ztnb      |      25|   2.1195|                2.44|                   1.0|                 0.0|       0.72|
|Wyoming              |hsgp     |hurdle_ztpoisson |      25|   2.1428|                2.48|                   1.0|                 0.0|       0.80|
|Wyoming              |sir      |hurdle_ztnb      |      25|   2.5200|                2.52|                   1.0|                 0.0|       0.44|
|Wyoming              |sir      |hurdle_ztpoisson |      25|   2.5200|                2.52|                   1.0|                 0.0|       0.44|

## State-facet plots

- `epidemic_comparison_plots/state_facets_epidemic_hurdle_ztnb.pdf`
- `epidemic_comparison_plots/state_facets_epidemic_hurdle_ztpoisson.pdf`
