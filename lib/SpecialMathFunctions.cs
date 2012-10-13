using System;

// Code copyright John D. Cook, used with permission

namespace SpecialMathFunctions
{
    public class Validate
    {
        /// <summary>
        /// Verify input is between 0 and i inclusive or THROW.
        /// </summary>
        /// <param name="p"></param>
        /// <param name="extra">Optional additional information to put in exception string</param>
        public static void IsProbability(double p, string extra = "")
        {
            if (p < 0.0)
            {
                string msg = string.Format("Error: Negative value given for probability. {0} {1}", p, extra);
                throw new ArgumentException(msg);
            }
            else if (p > 1.0)
            {
                string msg = string.Format("Error: Value greater than 1 given for a probability. {0} {1}", p, extra);
                throw new ArgumentException(msg);
            }
        }

        /// <summary>
        /// Verify input is larger than 0 or THROW.
        /// </summary>
        /// <param name="x"></param>
        /// <param name="extra">Optional additional information to put in exception string</param>
        public static void IsPositive(double x, string extra = "")
        {
            if (!(x > 0))
            {
                string msg = string.Format("Error: Expected a positive value but received {0}. {1}", x, extra);
                throw new ArgumentException(msg);
            }
        }

        /// <summary>
        /// Verify input is greater than or equal to 0 or THROW.
        /// </summary>
        /// <param name="x"></param>
        /// <param name="extra">Optional additional information to put in exception string</param>
        public static void IsNonNegative(double x, string extra = "")
        {
            if (!(x >= 0))
            {
                string msg = string.Format("Error: Expected a non-negative value but received {0}. {1}", x, extra);
                throw new ArgumentException(msg);
            }
        }

        /// <summary>
        /// Verify input is less than 0 or THROW.
        /// </summary>
        /// <param name="x"></param>
        /// <param name="extra">Optional additional information to put in exception string</param>
        public static void IsNegative(double x, string extra = "")
        {
            if (!(x < 0))
            {
                string msg = string.Format("Error: Expected a negative value but received {0}. {1}", x, extra);
                throw new ArgumentException(msg);
            }
        }

        /// <summary>
        /// Verify input is not geater than 0 or THROW.
        /// </summary>
        /// <param name="x"></param>
        /// <param name="extra">Optional additional information to put in exception string</param>
        public static void IsNonPositive(double x, string extra = "")
        {
            if (!(x <= 0))
            {
                string msg = string.Format("Error: Expected a non-positive value but received {0}. {1}", x, extra);
                throw new ArgumentException(msg);
            }
        }

        /// <summary>
        /// Verify first argument is greater than second argument or THROW.
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="extra">Optional additional information to put in exception string</param>
        public static void IsGreaterThan(double a, double b, string extra = "")
        {
            if (!(a > b))
            {
                string msg = string.Format("Error: Expected {0} to be greater than {1}. {2}", a, b, extra);
                throw new ArgumentException(msg);
            }
        }

        /// <summary>
        /// Verify first argument is at least as large as second argument or THROW.
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="extra">Optional additional information to put in exception string</param>
        public static void IsGreaterThanOrEqualTo(double a, double b, string extra = "")
        {
            if (!(a >= b))
            {
                string msg = string.Format("Error: Expected {0} to be greater than or equal to {1}. {2}", a, b, extra);
                throw new ArgumentException(msg);
            }
        }

        /// <summary>
        /// Verify first argument is less than second argument or THROW.
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="extra">Optional additional information to put in exception string</param>
        public static void IsLessThan(double a, double b, string extra = "")
        {
            if (!(a < b))
            {
                string msg = string.Format("Error: Expected {0} to be less than {1}. {2}", a, b, extra);
                throw new ArgumentException(msg);
            }
        }

        /// <summary>
        /// Verify first argument is no greater than second argument or THROW.
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <param name="extra">Optional additional information to put in exception string</param>
        public static void IsLessThanOrEqualTo(double a, double b, string extra = "")
        {
            if (!(a <= b))
            {
                string msg = string.Format("Error: Expected {0} to be less than or equal to {1}. {2}", a, b, extra);
                throw new ArgumentException(msg);
            }
        }

        /// <summary>
        /// Verify first argument is strictly between second and third arguments or THROW.
        /// </summary>
        /// <param name="x">Argument to validate</param>
        /// <param name="lower">Lower bound</param>
        /// <param name="upper">Upper bound</param>
        /// <param name="extra">Optional additional information to put in exception string</param>
        public static void IsStrictlyBetween(double x, double lower, double upper, string extra = "")
        {
            if (x <= lower)
            {
                string msg = string.Format("Error: Value is supposed to be strictly larger than {0} but received {1} {2}", lower, x, extra);
                throw new ArgumentException(msg);
            }
            else if (x >= upper)
            {
                string msg = string.Format("Error: Value is supposed to be strictly less than {0} but received {1} {2}", upper, x, extra);
                throw new ArgumentException(msg);
            }
        }
    }

    /// <summary>
    /// Mathematical special functions and other useful functions
    /// </summary>
    public class SpecialFunctions
    {
        const double LOG_2 = 0.69314718055994530941723212145818;
        const double SQRT2 = 1.4142135623730950488016887242097;
        const double ONE_OVER_SQRT_2 = 0.70710678118654752440084436210485;
        const double DBL_EPSILON = 2.22044604925031e-016;

        ///////////////////////////////////////////////////////////////////////////////
        ////// Gamma and related functions ////////////////////////////////////////////

        /// <summary>
        /// Gamma function
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static double Gamma(double x)
        {
            bool arg_was_not_positive = (x <= 0);

            // Depending on its size, we may alter x.  y holds the altered value.
            double y = x;

            double result;

            // We split the function domain into four intervals:
            // (-infinity, 0], (0, DBL_EPSILON), [DBL_EPSILON, 12), and (12, infinity)
            // If overflow would result at any point, we throw an exception.

            ///////////////////////////////////////////////////////////////////////////
            // First interval: (-infinity, 0]
            // Use the identity gamma(z)*gamma(1-z) = pi*csc(pi*z)
            // to reduce to positive case
            if (arg_was_not_positive)
            {
                // Gamma has a singularity at every non-positive integer
                if (Math.IEEERemainder(y, 1.0) == 0.0)
                {
                    string msg = string.Format("Gamma is undefined at non-positive integer arguments. Recieved {0}.", y);
                    throw new ArgumentException(msg);
                }
                y = -x;
            }

            ///////////////////////////////////////////////////////////////////////////
            // Second interval: (0, DBL_EPSILON)

            // NB: C# does not have a constant like C's DBL_EPSILON.
            // The constant double.Epsilon is *NOT* what you might expect.
            if (y < DBL_EPSILON)
                result = 1.0/y;

            ///////////////////////////////////////////////////////////////////////////
            // Third interval: [DBL_EPSILON, 12)
            else if (y < 12.0)
            {
                // The algorithm directly approximates gamma over (1,2) and uses
                // reduction identities to reduce other arguments to this interval.

                int n = 0;

                bool arg_was_less_than_one = (y < 1.0);

                // Add or subtract integers as necessary to bring y into (1,2)
                // Will correct for this below
                if (arg_was_less_than_one)
                {
                    y++;
                }
                else
                {
                    n =  Convert.ToInt32(Math.Floor(y)) - 1;  // will use n later
                    y -= n;
                }

                // numerator coefficients for approximation over the interval (1,2)
                double[] p = new double[]
                {
                    -1.71618513886549492533811E+0,
                     2.47656508055759199108314E+1,
                    -3.79804256470945635097577E+2,
                     6.29331155312818442661052E+2,
                     8.66966202790413211295064E+2,
                    -3.14512729688483675254357E+4,
                    -3.61444134186911729807069E+4,
                     6.64561438202405440627855E+4
                };

                // denominator coefficients for approximation over the interval (1,2)
                double[] q = new double[]
                {
                    -3.08402300119738975254353E+1,
                     3.15350626979604161529144E+2,
                    -1.01515636749021914166146E+3,
                    -3.10777167157231109440444E+3,
                     2.25381184209801510330112E+4,
                     4.75584627752788110767815E+3,
                    -1.34659959864969306392456E+5,
                    -1.15132259675553483497211E+5
                };

                double num = 0.0;
                double den = 1.0;
                int i;

                double z = y - 1;
                for (i = 0; i < 8; i++)
                {
                    num = (num + p[i])*z;
                    den = den*z + q[i];
                }
                result = num/den + 1.0;

                // Apply correction if argument was not initially in (1,2)
                if (arg_was_less_than_one)
                {
                    // Use identity gamma(z) = gamma(z+1)/z
                    // The variable "result" now holds gamma of the original y + 1
                    // Thus we use y-1 to get back the orginal y.
                    result /= (y-1.0);
                }
                else
                {
                    // Use the identity gamma(z+n) = z*(z+1)* ... *(z+n-1)*gamma(z)
                    for (i = 0; i < n; i++)
                        result *= y++;
                }
            }

            ///////////////////////////////////////////////////////////////////////////
            // Fourth interval: [12, infinity)
            else  // y > 12
            {
                // Abramowitz and Stegun 6.1.41
                // Asymptotic series should be good to at least 11 or 12 figures
                // For error analysis, see Whittiker and Watson
                // A Course in Modern Analysis (1927), page 252
                // <TeX> Note that W & W's $B_n$ corresponds to $B_{2n}$ in more modern notation </TeX>
                // We use the modern notation in the notes below.

                /*
                <TeX>
                $$ \log \Gamma(x) \sim (x-\frac{1}{2}) \log x - x - \frac{1}{2}\log 2\pi
                   + \sum_{j=1}^n \frac{ (-1)^n B_{2j} }{ 2j(2j-1) x^{2j-1} } $$
                   with error bounded by
                $$ \frac{ B_{2j+2} }{ 2(n+1)(2n + 1) \mid x \mid^2 } $$
                </TeX>
                */

                const double XBIG = 171.624;

                if (y > XBIG)
                {
                    string msg = string.Format
                    (
                        "Gamma argument is too large, must be less than or equal to {0}. Received {1}.",
                        XBIG,
                        y
                    );
                    throw new ArgumentOutOfRangeException(msg);
                }

                double[] c = new double[8]
                {
                       1.0/12.0    ,
                      -1.0/360.0   ,
                       1.0/1260.0  ,
                      -1.0/1680.0  ,
                       1.0/1188.0  ,
                    -691.0/360360.0,
                       1.0/156.0   ,
                   -3617.0/122400.0
                };
                double z = 1.0/(y*y);
                double sum = c[7];
                for (int i=6; i >= 0; i--)
                {
                    sum *= z;
                    sum += c[i];
                }
                double series = sum/y;

                double halfLogTwoPi = 0.5*Math.Log(2.0*Math.PI);
                double logGamma = (y - 0.5)*Math.Log(y) - y + halfLogTwoPi + series;

                result = Math.Exp(logGamma);
            }

            // Adjust result if orginal argument was negative
            if (arg_was_not_positive)
                result = Math.PI/(-x*result*Math.Sin(Math.PI*x));

            return result;
        }

        public static double LogGamma(double x)
        {
            Validate.IsNonNegative(x);

            if (x < 12.0)
            {
                return Math.Log(Math.Abs(Gamma(x)));
            }

            // Abramowitz and Stegun 6.1.41
            // Asymptotic series should be good to at least 11 or 12 figures
            // For error analysis, see Whittiker and Watson
            // A Course in Modern Analysis (1927), page 252

            double[] c =
            {
                 1.0/12.0,
                -1.0/360.0,
                1.0/1260.0,
                -1.0/1680.0,
                1.0/1188.0,
                -691.0/360360.0,
                1.0/156.0,
                -3617.0/122400.0
            };
            double z = 1.0/(x*x);
            double sum = c[7];
            for (int i=6; i >= 0; i--)
            {
                sum *= z;
                sum += c[i];
            }
            double series = sum/x;

            double halfLogTwoPi = 0.91893853320467274178032973640562;
            double logGamma = (x - 0.5)*Math.Log(x) - x + halfLogTwoPi + series;
            return logGamma;
        }

        /// <summary>
        /// Factorial. Is exact for return values that fit into an int. Uses table look-up.
        /// </summary>
        /// <param name="n"></param>
        /// <returns></returns>
        public static double Factorial(int n)
        {
            Validate.IsNonNegative(n);

            #region factorial table

            // Factorials of 0 to 39, in 35 decimal precision from Wolfram Alpha online (no
            //     Mathematica available, we po'!)
            //
            // 35 decimal precision chosen to guarantee correct evaluation in double-double
            //     and quadruple precision, although the representable range in quadruple
            //     (as opposed to double-double) precision is considerably larger than 170!.

            double[] factorialTable = {
                                          1.0000000000000000000000000000000000,      //   0!
                                          1.0000000000000000000000000000000000,      //   1!
                                          2.0000000000000000000000000000000000,      //   2!
                                          6.0000000000000000000000000000000000,      //   3!
                                          24.000000000000000000000000000000000,      //   4!
                                          120.00000000000000000000000000000000,      //   5!
                                          720.00000000000000000000000000000000,      //   6!
                                          5040.0000000000000000000000000000000,      //   7!
                                          40320.000000000000000000000000000000,      //   8!
                                          362880.00000000000000000000000000000,      //   9!
                                          //
                                          3.6288000000000000000000000000000000E+6,   //  10!
                                          3.9916800000000000000000000000000000E+7,   //  11!
                                          4.7900160000000000000000000000000000E+8,   //  12!
                                          6.2270208000000000000000000000000000E+9,   //  13!
                                          8.7178291200000000000000000000000000E+10,  //  14!
                                          1.3076743680000000000000000000000000E+12,  //  15!
                                          2.0922789888000000000000000000000000E+13,  //  16!
                                          3.5568742809600000000000000000000000E+14,  //  17!
                                          6.4023737057280000000000000000000000E+15,  //  18!
                                          1.2164510040883200000000000000000000E+17,  //  19!
                                          //
                                          2.4329020081766400000000000000000000E+18,  //  20!
                                          5.1090942171709440000000000000000000E+19,  //  21!
                                          1.1240007277776076800000000000000000E+21,  //  22!
                                          2.5852016738884976640000000000000000E+22,  //  23!
                                          6.2044840173323943936000000000000000E+23,  //  24!
                                          1.5511210043330985984000000000000000E+25,  //  25!
                                          4.0329146112660563558400000000000000E+26,  //  26!
                                          1.0888869450418352160768000000000000E+28,  //  27!
                                          3.0488834461171386050150400000000000E+29,  //  28!
                                          8.8417619937397019545436160000000000E+30,  //  29!
                                          //
                                          2.6525285981219105863630848000000000E+32,  //  30!
                                          8.2228386541779228177255628800000000E+33,  //  31!
                                          2.6313083693369353016721801216000000E+35,  //  32!
                                          8.6833176188118864955181944012800000E+36,  //  33!
                                          2.9523279903960414084761860964352000E+38,  //  34!
                                          1.0333147966386144929666651337523200E+40,  //  35!
                                          3.7199332678990121746799944815083520E+41,  //  36!
                                          1.3763753091226345046315979581580902E+43,  //  37!
                                          5.2302261746660111176000722410007429E+44,  //  38!
                                          2.0397882081197443358640281739902897E+46,  //  39!
                                          //
                                          8.1591528324789773434561126959611589E+47,  //  40!
                                          3.3452526613163807108170062053440752E+49,  //  41!
                                          1.4050061177528798985431426062445116E+51,  //  42!
                                          6.0415263063373835637355132068513998E+52,  //  43!
                                          2.6582715747884487680436258110146159E+54,  //  44!
                                          1.1962222086548019456196316149565772E+56,  //  45!
                                          5.5026221598120889498503054288002549E+57,  //  46!
                                          2.5862324151116818064296435515361198E+59,  //  47!
                                          1.2413915592536072670862289047373375E+61,  //  48!
                                          6.0828186403426756087225216332129538E+62,  //  49!
                                          //
                                          3.0414093201713378043612608166064769E+64,  //  50!
                                          1.5511187532873822802242430164693032E+66,  //  51!
                                          8.0658175170943878571660636856403767E+67,  //  52!
                                          4.2748832840600255642980137533893996E+69,  //  53!
                                          2.3084369733924138047209274268302758E+71,  //  54!
                                          1.2696403353658275925965100847566517E+73,  //  55!
                                          7.1099858780486345185404564746372495E+74,  //  56!
                                          4.0526919504877216755680601905432322E+76,  //  57!
                                          2.3505613312828785718294749105150747E+78,  //  58!
                                          1.3868311854568983573793901972038941E+80,  //  59!
                                          //
                                          8.3209871127413901442763411832233644E+81,  //  60!
                                          5.0758021387722479880085681217662523E+83,  //  61!
                                          3.1469973260387937525653122354950764E+85,  //  62!
                                          1.9826083154044400641161467083618981E+87,  //  63!
                                          1.2688693218588416410343338933516148E+89,  //  64!
                                          8.2476505920824706667231703067854963E+90,  //  65!
                                          5.4434493907744306400372924024784275E+92,  //  66!
                                          3.6471110918188685288249859096605464E+94,  //  67!
                                          2.4800355424368305996009904185691716E+96,  //  68!
                                          1.7112245242814131137246833888127284E+98,  //  69!
                                          //
                                          1.1978571669969891796072783721689099E+100, //  70!
                                          8.5047858856786231752116764423992601E+101, //  71!
                                          6.1234458376886086861524070385274673E+103, //  72!
                                          4.4701154615126843408912571381250511E+105, //  73!
                                          3.3078854415193864122595302822125378E+107, //  74!
                                          2.4809140811395398091946477116594034E+109, //  75!
                                          1.8854947016660502549879322608611466E+111, //  76!
                                          1.4518309202828586963407078408630828E+113, //  77!
                                          1.1324281178206297831457521158732046E+115, //  78!
                                          8.9461821307829752868514417153983165E+116, //  79!
                                          //
                                          7.1569457046263802294811533723186532E+118, //  80!
                                          5.7971260207473679858797342315781091E+120, //  81!
                                          4.7536433370128417484213820698940495E+122, //  82!
                                          3.9455239697206586511897471180120611E+124, //  83!
                                          3.3142401345653532669993875791301313E+126, //  84!
                                          2.8171041143805502769494794422606116E+128, //  85!
                                          2.4227095383672732381765523203441260E+130, //  86!
                                          2.1077572983795277172136005186993896E+132, //  87!
                                          1.8548264225739843911479684564554628E+134, //  88!
                                          1.6507955160908461081216919262453619E+136, //  89!
                                          //
                                          1.4857159644817614973095227336208257E+138, //  90!
                                          1.3520015276784029625516656875949514E+140, //  91!
                                          1.2438414054641307255475324325873553E+142, //  92!
                                          1.1567725070816415747592051623062404E+144, //  93!
                                          1.0873661566567430802736528525678660E+146, //  94!
                                          1.0329978488239059262599702099394727E+148, //  95!
                                          9.9167793487094968920957140154189380E+149, //  96!
                                          9.6192759682482119853328425949563699E+151, //  97!
                                          9.4268904488832477456261857430572425E+153, //  98!
                                          9.3326215443944152681699238856266700E+155, //  99!
                                          //
                                          9.3326215443944152681699238856266700E+157, // 100!
                                          9.4259477598383594208516231244829367E+159, // 101!
                                          9.6144667150351266092686555869725955E+161, // 102!
                                          9.9029007164861804075467152545817733E+163, // 103!
                                          1.0299016745145627623848583864765044E+166, // 104!
                                          1.0813967582402909005041013058003296E+168, // 105!
                                          1.1462805637347083545343473841483494E+170, // 106!
                                          1.2265202031961379393517517010387339E+172, // 107!
                                          1.3246418194518289744998918371218326E+174, // 108!
                                          1.4438595832024935822048821024627975E+176, // 109!
                                          //
                                          1.5882455415227429404253703127090773E+178, // 110!
                                          1.7629525510902446638721610471070758E+180, // 111!
                                          1.9745068572210740235368203727599249E+182, // 112!
                                          2.2311927486598136465966070212187151E+184, // 113!
                                          2.5435597334721875571201320041893352E+186, // 114!
                                          2.9250936934930156906881518048177355E+188, // 115!
                                          3.3931086844518982011982560935885732E+190, // 116!
                                          3.9699371608087208954019596294986306E+192, // 117!
                                          4.6845258497542906565743123628083842E+194, // 118!
                                          5.5745857612076058813234317117419772E+196, // 119!
                                          //
                                          6.6895029134491270575881180540903726E+198, // 120!
                                          8.0942985252734437396816228454493508E+200, // 121!
                                          9.8750442008336013624115798714482080E+202, // 122!
                                          1.2146304367025329675766243241881296E+205, // 123!
                                          1.5061417415111408797950141619932807E+207, // 124!
                                          1.8826771768889260997437677024916009E+209, // 125!
                                          2.3721732428800468856771473051394171E+211, // 126!
                                          3.0126600184576595448099770775270597E+213, // 127!
                                          3.8562048236258042173567706592346364E+215, // 128!
                                          4.9745042224772874403902341504126810E+217, // 129!
                                          //
                                          6.4668554892204736725073043955364853E+219, // 130!
                                          8.4715806908788205109845687581527957E+221, // 131!
                                          1.1182486511960043074499630760761690E+224, // 132!
                                          1.4872707060906857289084508911813048E+226, // 133!
                                          1.9929427461615188767373241941829484E+228, // 134!
                                          2.6904727073180504835953876621469804E+230, // 135!
                                          3.6590428819525486576897272205198933E+232, // 136!
                                          5.0128887482749916610349262921122539E+234, // 137!
                                          6.9177864726194884922281982831149104E+236, // 138!
                                          9.6157231969410890041971956135297254E+238, // 139!
                                          //
                                          1.3462012475717524605876073858941616E+241, // 140!
                                          1.8981437590761709694285264141107678E+243, // 141!
                                          2.6953641378881627765885075080372903E+245, // 142!
                                          3.8543707171800727705215657364933251E+247, // 143!
                                          5.5502938327393047895510546605503881E+249, // 144!
                                          8.0479260574719919448490292577980628E+251, // 145!
                                          1.1749972043909108239479582716385172E+254, // 146!
                                          1.7272458904546389112034986593086202E+256, // 147!
                                          2.5563239178728655885811780157767579E+258, // 148!
                                          3.8089226376305697269859552435073693E+260, // 149!
                                          //
                                          5.7133839564458545904789328652610540E+262, // 150!
                                          8.6272097742332404316231886265441915E+264, // 151!
                                          1.3113358856834525456067246712347171E+267, // 152!
                                          2.0063439050956823947782887469891172E+269, // 153!
                                          3.0897696138473508879585646703632405E+271, // 154!
                                          4.7891429014633938763357752390630227E+273, // 155!
                                          7.4710629262828944470838093729383154E+275, // 156!
                                          1.1729568794264144281921580715513155E+278, // 157!
                                          1.8532718694937347965436097530510785E+280, // 158!
                                          2.9467022724950383265043395073512149E+282, // 159!
                                          //
                                          4.7147236359920613224069432117619438E+284, // 160!
                                          7.5907050539472187290751785709367295E+286, // 161!
                                          1.2296942187394494341101789284917502E+289, // 162!
                                          2.0044015765453025775995916534415528E+291, // 163!
                                          3.2872185855342962272633303116441466E+293, // 164!
                                          5.4239106661315887749844950142128418E+295, // 165!
                                          9.0036917057784373664742617235933175E+297, // 166!
                                          1.5036165148649990402012017078400840E+300, // 167!
                                          2.5260757449731983875380188691713411E+302, // 168!
                                          4.2690680090047052749392518888995665E+304, // 169!
                                          //
                                          7.2574156153079989673967282111292631E+306  // 170!
                                      };

            #endregion factorial table

            int MAX_FACTORIAL_ARGUMENT = factorialTable.GetUpperBound(0);
            if (n > MAX_FACTORIAL_ARGUMENT)
            {
                string msg = string.Format
                (
                    "Maximum factorial argument {0}, received {1}.",
                    MAX_FACTORIAL_ARGUMENT,
                    n
                );
                throw new ArgumentOutOfRangeException(msg);
            }
            return factorialTable[n];
        }

        /// <summary>
        /// The logorithm of the Beta function.
        /// </summary>
        /// <param name="a"></param>
        /// <param name="b"></param>
        /// <returns></returns>

        ///////////////////////////////////////////////////////////////////////////////
        ////// Polygamma functions ////////////////////////////////////////////////////

        /// <summary>
        /// Also known as Psi(x). Derivative of the log of the gamma function.
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static double DiGamma(double x)
        {
            // Algorithm source:
            // Applied Statistics (1976) vol.25, no.3
            // Algorithm AS 103

            // Should be accurate to 10 figures

            const double small_argument_cutoff = 1.0e-5;
            const double large_argument_cutoff = 8.5;
            const double eulers_constant       = 0.5772156649;

            double s3 = 1.0/12.0;  // 3rd Stirling coefficient
            double s4 = 1.0/120.0; // 4th Stirling coefficient
            double s5 = 1.0/252.0; // 5th Stirling coefficient

            double retval = 0.0;

            Validate.IsPositive(x);

            if (x < small_argument_cutoff)
                return -eulers_constant - 1.0/x;

            // Reduce to diGamma(x+n) where x+n >= large_argument_cutoff
            while (x < large_argument_cutoff)
                retval -= 1.0/x++;

            // Use Stirling's (actually de Moivre's) expansion if argument > C
            double r = 1.0/x;
            retval += Math.Log(x) - 0.5*r;
            r *= r;
            retval -= r*(s3 - r*(s4 - r*s5));
            return retval;
        }

        /// <summary>
        /// Second derivative of the log gamma function, first derivative of Psi(x).
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static double TriGamma(double x)
        {
            // Algorithm taken from
            // Applied Statistics (1978) vol.27, no.1
            // Algorithm AS 121

            // Should be accurate to 8 figures

            const double small_argument_cutoff = 1.0e-4;
            const double large_argument_cutoff = 5.0;

            // Bernoulli numbers
            double b2 =  1.0/6.0;
            double b4 = -1.0/30.0;
            double b6 =  1.0/42.0;
            double b8 = -1.0/30.0;

            double retval = 0.0;
            Validate.IsPositive(x);

            if (x < small_argument_cutoff)
                return 1.0/(x*x);

            // Reduce to triGamma(x+n) where x+n >= large_argument_cutoff
            while (x < large_argument_cutoff)
            {
                retval += 1.0/(x*x);
                x++;
            }

            // Apply asymptotic formula for x >= large_argument_cutoff
            double y = 1.0/(x*x);
            retval += 0.5*y + (1.0 + y*(b2 + y*(b4 + y*(b6 + y*b8)))) / x;
            return retval;
        }

        /// <summary>
        /// Third derivative of the log gamma function.
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static double TetraGamma(double x)
        {
            // Ported from Dario Alpern's complex number calculator
            // http://www.alpertron.com.ar/CALC.HTM

            Validate.IsPositive(x);

            double c1 = x + 7.0;

            double[] coeff = new double []
            {
                 1.0/2.0,
                 1.0/3.0,
                 2.0/3.0,
                 6.0/5.0,
                 9.0/5.0,
                18.0/7.0,
                24.0/7.0
            };

            int j;
            double s = 3.0/c1;
            for( j = 6; j >= 0; j-- )
            {
                s = coeff[j]/(c1+s);
            }

            s = -((s + 1.0)/c1 + 1.0)/(c1*c1);

            for (j = 0; j < 7; j++)
            {
                c1--;
                s -= 2.0/(c1*c1*c1);
            }

            return s;
        }

        ///////////////////////////////////////////////////////////////////////////////
        ////// Functions to avoid cancellation or overflow ////////////////////////////

        /// <summary>
        /// Test whether argument is not a NaN and is not infinite
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static bool IsFiniteNumber(double x)
        {
            return (!double.IsNaN(x) && !double.IsInfinity(x));
        }

        /// <summary>
        /// Compute ln(1+x) avoiding loss of precision for x near 0.
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static double LogOnePlusX(double x)
        {

            // Taken from DCDFLIB's alnrel, fortran code translated to C */

            if (x <= -1.0)
            {
                string msg = string.Format("Argument must be greater than -1. Received {0}.", x);
                throw new ArgumentOutOfRangeException(msg);
            }

            if (Math.Abs(x) > 0.375)
            {
                // x is sufficiently large that the obvious evaluation is OK
                return Math.Log(1.0 + x);
            }

            const double p1 =  -.129418923021993e+01;
            const double p2 =   .405303492862024e+00;
            const double p3 =  -.178874546012214e-01;
            const double q1 =  -.162752256355323e+01;
            const double q2 =   .747811014037616e+00;
            const double q3 =  -.845104217945565e-01;
            double t, t2, w;

            t = x/(x+2.0);
            t2 = t*t;
            w = (((p3*t2+p2)*t2+p1)*t2+1.0)/(((q3*t2+q2)*t2+q1)*t2+1.0);
            return 2.0*t*w;
        }

        /// <summary>
        /// This function prevents loss of precision for e^-1 when x is close to 0
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static double ExpMinusOne(double x)
        {
            // Taken from DCDFLIB.

            /*
            -----------------------------------------------------------------------
                        EVALUATION OF THE FUNCTION EXP(X) - 1
            -----------------------------------------------------------------------
            */
            const double p1 =  .914041914819518e-09;
            const double p2 =  .238082361044469e-01;
            const double q1 = -.499999999085958e+00;
            const double q2 =  .107141568980644e+00;
            const double q3 = -.119041179760821e-01;
            const double q4 =  .595130811860248e-03;
            double rexp = 0.0;

            if( !(Math.Abs(x) > 0.15e0) )
            {
                rexp = x*(((p2*x+p1)*x+1.0e0)/((((q4*x+q3)*x+q2)*x+q1)*x+1.0e0));
                return rexp;
            }

            double w = Math.Exp(x);
            if( !(x > 0.0e0) )
            {
                rexp = w - 0.5e0 - 0.5e0;
                return rexp;
            }

            rexp = w * ( 0.5e0 + ( 0.5e0 - 1.0e0/w ));
            return rexp;
        }

        /// <summary>
        /// Compute sqrt(x*x + y*y) avoiding overflow for large arguments.
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        public static double Hypotenuse(double x, double y)
        {
            x = Math.Abs(x);
            y = Math.Abs(y);
            double min = Math.Min(x, y);
            double max = Math.Max(x, y);
            double r = min / max;
            return max * Math.Sqrt(1 + r * r);
        }

        /// <summary>
        /// Makes source code easier to read when squaring a large expression.
        /// More efficient than calling Math.Pow().
        /// </summary>
        /// <param name="x"></param>
        /// <returns></returns>
        public static double Square(double x)
        {
            return x*x;
        }

        public static double Erf(double x)
        {
            // constants
            double a1 = 0.254829592;
            double a2 = -0.284496736;
            double a3 = 1.421413741;
            double a4 = -1.453152027;
            double a5 = 1.061405429;
            double p = 0.3275911;

            // Save the sign of x
            int sign = 1;
            if (x < 0)
                sign = -1;
            x = Math.Abs(x);

            // A&S formula 7.1.26
            double t = 1.0 / (1.0 + p*x);
            double y = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*Math.Exp(-x*x);

            return sign*y;
        }

        ///////////////////////////////////////////////////////////////////////////////
        ////// Utility functions //////////////////////////////////////////////////////

        /// <summary>
        /// Transfers the sign of the variable "sign" to the variable "mag"
        /// </summary>
        /// <param name="mag"></param>
        /// <param name="sign"></param>
        /// <returns></returns>
        public static double CopySign(double mag, double sign)
        {
            // Known as "fifdsign" in Promula-translated fortran
            if (mag < 0) mag = -mag;
            if (sign < 0) mag = -mag;
            return mag;
        }

        static double NormalInverseRationalApproximation(double t)
        {
            // Abramowitz and Stegun formula 26.2.23.
            // The absolute value of the error should be less than 4.5 e-4.
            double[] c = new double[] { 2.515517, 0.802853, 0.010328 };
            double[] d = new double[] { 1.432788, 0.189269, 0.001308 };
            return t - ((c[2] * t + c[1]) * t + c[0]) /
                       (((d[2] * t + d[1]) * t + d[0]) * t + 1.0);
        }
    }
}