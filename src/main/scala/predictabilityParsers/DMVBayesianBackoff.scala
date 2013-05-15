package runDMV.predictabilityParsers

//import akka.actor.Actor
import akka.dispatch.{ ExecutionContext, Future }
import java.util.concurrent.Executors
import joptsimple.OptionParser;
import joptsimple.OptionSet;
//import predictabilityParsing.util.CorpusManipulation
import predictabilityParsing.parsers.{VanillaDMVEstimator,VanillaDMVParser}
import predictabilityParsing.grammars.DMVBayesianBackoffGrammar
// import predictabilityParsing.grammars.DMVBayesianBackoffFullyConditionalGrammar
// import predictabilityParsing.grammars.DMVBayesianBackoffFullyConditionalAndBackedOffGrammar
import predictabilityParsing.grammars.DMVBayesianBackoffIndependentDepsGrammar
import predictabilityParsing.grammars.DMVBayesianBackoffJointDepsGrammar
import predictabilityParsing.types.labels._
import scala.language.postfixOps

object DMVBayesianBackoff {
  def main( args:Array[String]) {

    val optsParser = new OptionParser()

    optsParser.accepts( "trainStrings" ).withRequiredArg
    optsParser.accepts( "testStrings" ).withRequiredArg
    optsParser.accepts( "grammarInit" ).withRequiredArg
    optsParser.accepts( "randomSeed" ).withRequiredArg
    optsParser.accepts( "grammarInitMinLength" ).withRequiredArg
    optsParser.accepts( "rightFirst" ).withRequiredArg
    optsParser.accepts( "cAttach" ).withRequiredArg
    optsParser.accepts( "cStop" ).withRequiredArg
    optsParser.accepts( "cNotStop" ).withRequiredArg
    optsParser.accepts( "stopUniformity" ).withRequiredArg
    optsParser.accepts( "uniformRoot" )
    optsParser.accepts( "evalFreq" ).withRequiredArg
    optsParser.accepts( "unkCutoffA" ).withRequiredArg
    optsParser.accepts( "unkCutoffB" ).withRequiredArg
    optsParser.accepts( "convergence" ).withRequiredArg
    optsParser.accepts( "minIter" ).withRequiredArg
    optsParser.accepts( "maxMarginalParse" )
    optsParser.accepts( "babySteps" )
    optsParser.accepts( "slidingBabySteps" )
    optsParser.accepts( "emissionType" ).withRequiredArg
    optsParser.accepts( "noBackoffAlpha" ).withRequiredArg
    optsParser.accepts( "backoffAlpha" ).withRequiredArg
    optsParser.accepts( "dmvRulesAlpha" ).withRequiredArg
    optsParser.accepts( "stopAlpha" ).withRequiredArg
    optsParser.accepts( "chooseAlpha" ).withRequiredArg
    optsParser.accepts( "printFinalGrammar" )
    optsParser.accepts( "annotateTestSet" )
    optsParser.accepts( "printFinalPartialCounts" )
    optsParser.accepts( "printFinalPartialCountsEachUtt" )
    optsParser.accepts( "printArcProbabilities" )
    optsParser.accepts( "printStopProbabilities" )
    optsParser.accepts( "posteriorMode" )
    optsParser.accepts( "posteriorMean" )
    optsParser.accepts( "printSpanProbabilities" )
    optsParser.accepts( "foldUnfold" )

    val opts = optsParser.parse( args:_* )

    val trainStrings = opts.valueOf( "trainStrings" ).toString
    val testStrings = opts.valueOf( "testStrings" ).toString

    val grammarInit =
      if(opts.has( "grammarInit" )) opts.valueOf( "grammarInit" ).toString else "hardlineStopHarmonicGrammar"

    val randomSeed =
      if(opts.has( "randomSeed" )) opts.valueOf( "randomSeed" ).toString.toDouble
      else 15D

    val grammarInitMinLength =
      if(opts.has( "grammarInitMinLength" )) opts.valueOf( "grammarInitMinLength" ).toString.toInt
      else 1


    val rightFirst =
      if(opts.has( "rightFirst" )) opts.valueOf( "rightFirst" ).toString.toDouble else 0.75

    val cAttach =
      if(opts.has( "cAttach" )) opts.valueOf( "cAttach" ).toString.toDouble else 15.0

    val cStop =
      if(opts.has( "cStop" ) ) opts.valueOf( "cStop" ).toString.toDouble else 3.0

    val cNotStop =
      if(opts.has( "cNotStop" )) opts.valueOf( "cNotStop" ).toString.toDouble else 1.0

    val stopUniformity =
      if(opts.has( "stopUniformity" )) opts.valueOf( "stopUniformity" ).toString.toDouble else 20.0

    val uniformRoot = opts.has( "uniformRoot" )

    val evalFreq =
      if(opts.has( "evalFreq" )) opts.valueOf( "evalFreq" ).toString.toInt else 4

    val unkCutoffA =
      if(opts.has( "unkCutoffA" )) opts.valueOf( "unkCutoffA" ).toString.toInt else 5

    val unkCutoffB =
      if(opts.has( "unkCutoffB" )) opts.valueOf( "unkCutoffB" ).toString.toInt else 5

    val minIter =
      if(opts.has( "minIter" )) opts.valueOf( "minIter" ).toString.toDouble else 1D

    val convergence =
      if(opts.has( "convergence" )) opts.valueOf( "convergence").toString.toDouble else 0.00001

    val maxMarginalParse = opts.has( "maxMarginalParse" )

    val babySteps = if( opts.has( "babySteps" ) ) 0.00001D else 0.0D

    val slidingBabySteps = if( opts.has( "slidingBabySteps" ) ) 25 else 0

    val emissionType =
      if( opts.has( "emissionType" ) ) opts.valueOf( "emissionType" ).toString else "streamB"

    var noBackoffAlpha =
      if( opts.has("noBackoffAlpha") ) opts.valueOf( "noBackoffAlpha").toString.toDouble
      else 35D

    var backoffAlpha =
      if( opts.has("backoffAlpha") ) opts.valueOf( "backoffAlpha").toString.toDouble
      else 70D

    var dmvRulesAlpha =
      if( opts.has("dmvRulesAlpha") ) opts.valueOf( "dmvRulesAlpha").toString.toDouble
      else 1D

    var stopAlpha =
      if( opts.has("stopAlpha") ) opts.valueOf( "stopAlpha").toString.toDouble
      else dmvRulesAlpha

    var chooseAlpha =
      if( opts.has("chooseAlpha") ) opts.valueOf( "chooseAlpha").toString.toDouble
      else dmvRulesAlpha

    val printFinalGrammar = opts.has( "printFinalGrammar" )

    val annotateTestSet = opts.has( "annotateTestSet" )

    val printFinalPartialCounts = opts.has( "printFinalPartialCounts" )

    val printFinalPartialCountsEachUtt = opts.has( "printFinalPartialCountsEachUtt" )

    val printArcProbabilities = opts.has( "printArcProbabilities" )

    val printStopProbabilities = opts.has( "printStopProbabilities" )

    val posteriorMode = opts.has( "posteriorMode" )

    val posteriorMean = opts.has( "posteriorMean" )
        // var stopBackoffAlpha =
        //   if( opts.has("stopBackoffAlpha") ) opts.valueOf( "stopBackoffAlpha").toString.toDouble
        //   else 70D

        // var chooseNoBackoffAlpha =
        //   if( opts.has("chooseNoBackoffAlpha") ) opts.valueOf( "chooseNoBackoffAlpha").toString.toDouble
        //   else 30D

        // var chooseBackoffHeadAlpha =
        //   if( opts.has("chooseBackoffHeadAlpha") ) opts.valueOf( "chooseBackoffHeadAlpha").toString.toDouble
        //   else 60D

        // var chooseBackoffArgAlpha =
        //   if( opts.has("chooseBackoffArgAlpha") ) opts.valueOf( "chooseBackoffArgAlpha").toString.toDouble
        //   else 60D

        // var chooseBackoffBothAlpha =
        //   if( opts.has("chooseBackoffBothAlpha") ) opts.valueOf( "chooseBackoffBothAlpha").toString.toDouble
        //   else 120D

    val printSpanProbabilities = opts.has( "printSpanProbabilities" )

    val foldUnfold = opts.has( "foldUnfold" )

    println( "trainStrings: " + trainStrings )
    println( "testStrings: " + testStrings )
    println( "grammarInit: " + grammarInit )
    println( "randomSeed: " + randomSeed )
    println( "grammarInitMinLength: " + grammarInitMinLength )
    println( "rightFirst: " + rightFirst )
    println( "cAttach: " + cAttach )
    println( "cStop: " + cStop )
    println( "cNotStop: " + cNotStop )
    println( "stopUniformity: " + stopUniformity )
    println( "uniformRoot: " + uniformRoot )
    println( "evalFreq: " + evalFreq )
    println( "unkCutoffA: " + unkCutoffA )
    println( "unkCutoffB: " + unkCutoffB )
    println( "minIter: " + minIter )
    println( "convergence: " + convergence )
    println( "maxMarginalParse: " + maxMarginalParse )
    println( "babySteps: " + babySteps )
    println( "slidingBabySteps: " + slidingBabySteps )
    println( "emissionType: " + emissionType )
    println( "noBackoffAlpha: " + noBackoffAlpha )
    println( "backoffAlpha: " + backoffAlpha )
    println( "dmvRulesAlpha: " + dmvRulesAlpha )
    println( "stopAlpha: " + stopAlpha )
    println( "chooseAlpha: " + chooseAlpha )
    println( "printFinalGrammar: " + printFinalGrammar )
    println( "annotateTestSet: " + annotateTestSet )
    println( "printFinalPartialCounts: " + printFinalPartialCounts )
    println( "printFinalPartialCountsEachUtt: " + printFinalPartialCountsEachUtt )
    println( "printArcProbabilities: " + printArcProbabilities )
    println( "printStopProbabilities: " + printStopProbabilities )
    println( "posteriorMode: " + posteriorMode )
    println( "posteriorMean: " + posteriorMean )
    println( "printSpanProbabilities: " + printSpanProbabilities )
    println( "foldUnfold: " + foldUnfold )


    print( "Reading in training set...." )
    val findRareWordsA = collection.mutable.Map[Word,Int]().withDefaultValue( 0 );
    val findRareWordsB = collection.mutable.Map[Word,Int]().withDefaultValue( 0 );
    var trainSet = io.Source.fromFile( trainStrings ).getLines.toList.map{ line =>
      val fields = line.split( " " ).toList
      ( (fields tail) zip (0 to ( fields.length-2 )) ).map{ case( s, t ) =>
        val wordParts = s.split( "#" );

        val wp = WordPair( wordParts(0), wordParts(1) )

        findRareWordsA( Word( wordParts(0) ) ) = 1 + findRareWordsA.getOrElse( Word( wordParts(0) ), 0 )
        findRareWordsB( Word( wordParts(1) ) ) = 1 + findRareWordsB.getOrElse( Word( wordParts(1) ), 0 )

        new TimedWordPair( wordParts(0), wordParts(1), t)
      }.toList
    }
    trainSet = trainSet.map( s =>
      s.map{ case TimedWordPair( w1, w2, t ) =>
        new TimedWordPair(
          { if( ( findRareWordsA( Word( w1 ) ) <= unkCutoffA ) && unkCutoffA > 0 ) "UNK" else w1 },
          { if( ( findRareWordsB( Word( w2 ) ) <= unkCutoffB ) && unkCutoffB > 0 ) "UNK" else w2 },
          t
        )
      }
    )
    println( " Done; " + trainSet.size + " training set strings" )

    print( "Reading in test set...." )
    val testSet = io.Source.fromFile( testStrings ).getLines.toList.map{ line =>
      val fields = line.split( " " ).toList
      TimedTwoStreamSentence(
        fields head,
        ( (fields tail) zip (0 to ( fields.length-2 )) ).map{ case( s,t ) =>

          val wordParts = s.split( "#" );
          val wp = WordPair( wordParts(0), wordParts(1) )

          new TimedWordPair(
            { if( ( findRareWordsA( Word( wordParts(0) ) ) <= unkCutoffA ) && unkCutoffA > 0 ) "UNK" else wordParts(0) },
            { if( ( findRareWordsB( Word( wordParts(1) ) ) <= unkCutoffB ) && unkCutoffB > 0 ) "UNK" else wordParts(1) },
            t
          )

          // if( findRareWords.getOrElse( Word( wordParts(0) ), 0 )  <= unkCutoff ) {
          //   //println( "Considering " + wp + " as UNK" )

          //   new TimedWordPair( "UNK", wordParts(1), t )

          // } else {
          //   new TimedWordPair( wordParts(0), wordParts(1), t)
          // }
        }.toList
      )
    }
    println( " Done; " + testSet.size + " test set strings" )

    // print( "training set: \n" + trainSet.map{_.mkString("", " ","")}.mkString("\n","\n","\n\n\n") )
    // print( "test set: \n" + testSet.mkString("\n","\n","\n\n\n") )

    val vocab = ( trainSet ++ testSet.map{ _.sentence } ).flatMap{ _.map{_.wp} }.toSet
    val streamBVocab = ( trainSet ++ testSet.map{ _.sentence } ).flatMap{ _.map{_.wp.obsB} }.toSet

    if( noBackoffAlpha == 0D && backoffAlpha == 0D ) {

      println( "Calibrating alphas to vocab size" )

      noBackoffAlpha = 1*streamBVocab.size ///10D
      backoffAlpha = 2*streamBVocab.size ///10D

      // chooseNoBackoffAlpha = 1*streamBVocab.size ///10D
      // chooseBackoffArgAlpha = 1.5*streamBVocab.size ///10D
      // chooseBackoffHeadAlpha = 1.5*streamBVocab.size ///10D
      // chooseBackoffBothAlpha = 2*streamBVocab.size ///10D

      println( "noBackoffAlpha: " + noBackoffAlpha )
      println( "backoffAlpha: " + backoffAlpha )
      // println( "chooseNoBackoffAlpha: " + chooseNoBackoffAlpha )
      // println( "chooseBackoffHeadAlpha: " + chooseBackoffHeadAlpha )
      // println( "chooseBackoffArgAlpha: " + chooseBackoffArgAlpha )
      // println( "chooseBackoffBothAlpha: " + chooseBackoffBothAlpha )
    }

    // This is the magic line... it should be enough to get this estimator relying on two stream
    // heads and stream A args...
    val estimator = new VanillaDMVEstimator {//( vocab )
      override val g =
        emissionType match {
          case "streamB" =>
            new DMVBayesianBackoffGrammar(
              noBackoffAlpha,
              backoffAlpha,
              //dmvRulesAlpha
              stopAlpha,
              chooseAlpha
            )
          // case "fullyConditional" =>
          //   new DMVBayesianBackoffFullyConditionalGrammar(
          //     noBackoffAlpha,
          //     backoffAlpha,
          //     dmvRulesAlpha
          //   )
          // case "fullyConditionalAndBackedOff" =>
          //   new DMVBayesianBackoffFullyConditionalAndBackedOffGrammar(
          //     noBackoffAlpha,
          //     backoffAlpha,
          //     dmvRulesAlpha
          //   )
          case "joint" =>
            new DMVBayesianBackoffJointDepsGrammar(
              noBackoffAlpha,
              backoffAlpha,
              dmvRulesAlpha
            )
          case "independent" =>
            new DMVBayesianBackoffIndependentDepsGrammar(
              noBackoffAlpha,
              backoffAlpha,
              dmvRulesAlpha
            )
        }
    }
    //estimator.set

    //estimator.setGrammar( new DMVTwoStreamHeadsGrammar ) //( vocab ) )

    if( grammarInit == "hardlineStopHarmonicGrammar" ) {
      print( "Initializing hardline stop harmonic grammar..." )
      estimator.setHardlineStopHarmonicGrammar(
        trainSet.filter{ _.length >= grammarInitMinLength },
        cAttach = cAttach,
        cStop = cStop,
        cNotStop = cNotStop,
        stopUniformity = stopUniformity,
        uniformRoot = uniformRoot
      )
    } else if ( grammarInit == "gradedStopHarmonicGrammar" ) {
      print( "Initializing graded stop harmonic grammar..." )
      estimator.setGradedStopHarmonicGrammar(
        trainSet.filter{ _.length >= grammarInitMinLength },
        cAttach = cAttach,
        cStop = cStop,
        cNotStop = cNotStop,
        stopUniformity = stopUniformity
      )
    } else if ( grammarInit == "uniformStopHarmonicGrammar" ) {
      print( "Initializing uniform stop harmonic grammar..." )
      estimator.setUniformStopHarmonicGrammar(
        trainSet.filter{ _.length >= grammarInitMinLength },
        cAttach = cAttach
      )
    } else if ( grammarInit == "priors" ) {
      print( "Init to priors..." )
      estimator.g.setUniform(vocab)
      //estimator.setGrammar( estimator.g.emptyPartialCounts.toDMVGrammar )
    } else {
      print( "Initializing random grammar..." )
      estimator.g.randomize(vocab,randomSeed)
    }

    println( " done" )

    // println( "Initial grammar:\n\n" + estimator.g )


    // val viterbiParser = new VanillaDMVParser
    // viterbiParser.setGrammar( estimator.g )

    val pool = Executors.newCachedThreadPool()
    implicit val ec = ExecutionContext.fromExecutorService( pool )

    if( evalFreq != 0 ) {
      if( maxMarginalParse ) {
        println( estimator.maxMarginalParse(testSet, "initial").mkString("\n", "\n", "\n"))
      } else {
        Future {
          val viterbiParser = new VanillaDMVParser { override val g = estimator.g }
          //viterbiParser.setGrammar( estimator.g )
          println( viterbiParser.bothParses(testSet, "initial").mkString("\n", "\n", "\n"))
        }
      }
    }

    var deltaLogProb = 1D
    var lastCorpusLogProb = 1D
    var iter = 0

    var deltaFreeEnergy = 1D
    var lastGrammarFreeEnergy = 1D

    //var thisIterMaxSentLength = 3
    //var thisIterMaxSentLength = trainSet.map{ _.length}.sortWith( _ < _).head
    val longestSentenceLength = trainSet.map{ _.length}.max
    val shortestSentenceLength = trainSet.map{ _.length}.min
    var slidingWindowLength:Int = 1
    var thisIterMaxSentLength = if( babySteps == 0 ) longestSentenceLength else shortestSentenceLength

    // var thisIterTrain =
    //   if( babySteps == 0D )
    //     trainSet
    //   else
    //     trainSet.filter{ _.length <= thisIterMaxSentLength }

    var thisIterTrain =
      if( babySteps > 0D )
        trainSet.filter{ _.length <= thisIterMaxSentLength }
      else if( slidingBabySteps > 0 )
        trainSet.flatMap{ s =>
          if( s.length <= slidingWindowLength )
            s::Nil
          else
            (0 to (s.length- slidingWindowLength) ).map{ i =>
              s.slice( i, i+slidingWindowLength )
            }
          }
        //trainSet.flatten
      else
        trainSet

    println( "Beginning VBEM" )
    while(
      math.abs( deltaLogProb ) > convergence ||
      deltaLogProb == (0D/0D) ||
      iter < minIter
    ) {
      val newPC = if(foldUnfold)
        estimator.efficientComputePartialCounts( thisIterTrain )
      else
        estimator.computePartialCounts( thisIterTrain )

      val corpusLogProb = newPC.getTotalScore
      deltaLogProb = ( ( lastCorpusLogProb - corpusLogProb ) / lastCorpusLogProb )

      println( "Iteration " + iter + ": " + corpusLogProb + " (" + deltaLogProb + ")" )
      println(
        thisIterMaxSentLength + " max sent length producing " + thisIterTrain.size + " sentences"
      )

      val newGrammar =
        if( babySteps > 0 ) {
          println( "baby steps grammar" )
          newPC.toDMVGrammar()
        } else if( slidingBabySteps > 0 ) {
          println( "sliding baby steps grammar" )
          newPC.toDMVGrammar()
        } else {
          newPC.toDMVGrammar()
        }

      estimator.setGrammar( newGrammar )

      if( evalFreq != 0 && iter%evalFreq == 0 && babySteps == 0 && slidingBabySteps == 0) {
        val iterLabel = "it" + iter
        //Actor.spawn {
          if( maxMarginalParse ) {
            println( estimator.maxMarginalParse(testSet, "it" + iter ).mkString("\n", "\n", "\n"))
          } else {
            Future {
              val viterbiParser = new VanillaDMVParser {
                override val g =
                  if( posteriorMode )
                    newPC.toDMVGrammar( posteriorMode = true )
                  else if( posteriorMean )
                    newPC.toDMVGrammar( posteriorMean = true )
                  else
                    estimator.g
              }
              println( viterbiParser.bothParses(testSet, "it" + iter ).mkString("\n", "\n", "\n"))
            }
          }
        }

      if(
        math.abs( deltaLogProb ) <= convergence &&
        thisIterMaxSentLength < longestSentenceLength
      ) {
        if( babySteps > 0D ) {
          println(
            "Jumping from maxlength of " + thisIterMaxSentLength + " to " + (thisIterMaxSentLength+1)
          )
          thisIterMaxSentLength += 1
          thisIterTrain = trainSet.filter{ _.length <= thisIterMaxSentLength }

          deltaLogProb = 1D
          lastCorpusLogProb = 1D
          // deltaFreeEnergy = 1D
          // lastGrammarFreeEnergy = 1D
        } else if( slidingBabySteps > 0 && slidingWindowLength < longestSentenceLength ) {
          println(
            "Jumping from sliding window size of " + slidingWindowLength +
              " to " + (slidingWindowLength+1)
          )

          val iterLabel = "window"+slidingWindowLength+"Converged"

          if( maxMarginalParse ) {
            // val viterbiParser = new VanillaDMVEstimator {
            //   override val g = estimator.g
            // }
            println( estimator.maxMarginalParse(testSet, iterLabel ).mkString("\n", "\n", "\n"))
          } else {
            Future {
              val viterbiParser = new VanillaDMVParser {
                override val g =
                  if( posteriorMode )
                    newPC.toDMVGrammar( posteriorMode = true )
                  else if( posteriorMean )
                    newPC.toDMVGrammar( posteriorMean = true )
                  else
                    estimator.g
              }
              //viterbiParser.setGrammar( estimator.g )
              println( viterbiParser.bothParses(testSet, iterLabel ).mkString("\n", "\n", "\n"))
            }
          }

          slidingWindowLength += 1
          thisIterTrain = trainSet.flatMap{ s =>
            if( s.length <= slidingWindowLength )
              s::Nil
            else
              (0 to (s.length - slidingWindowLength) ).map{ i =>
                s.slice( i, i+slidingWindowLength )
              }
          }

          println( "New training set contains " + thisIterTrain.size + " items" )

          //estimator.g.laplaceSmooth( 0.0001, thisIterTrain.flatten.map{_.w}.toSet )

          //println( estimator.g.stopScore( estimator.g.p_stop.parents.head , Stop) )
          // println( "\n\n\nAFTER SMOOTHING\n\n\n" )
          // println( estimator.g )
          // println( estimator.g.stopScore( estimator.g.p_stop.parents.head , Stop) )

          // println( thisIterTrain.flatten.toSet.mkString( "\t\t","\n\t\t","\n\n-----\n\n" ) )

          deltaLogProb = 1D
          lastCorpusLogProb = 1D
          // deltaFreeEnergy = 1D
          // lastGrammarFreeEnergy = 1D
        }
      }
      iter += 1
      lastCorpusLogProb = corpusLogProb
      // lastGrammarFreeEnergy = newFreeEnergy

      if( 
        !( math.abs( deltaLogProb ) > convergence ||
          deltaLogProb == (0D/0D) ||
          iter < minIter
        ) && printFinalPartialCounts
      ) {
        print( newPC )
      }
    }

    if( posteriorMode )
      estimator.setGrammar(
        estimator.computePartialCounts( thisIterTrain ).toDMVGrammar( posteriorMode = true )
      )
    else if( posteriorMean )
      estimator.setGrammar(
        estimator.computePartialCounts( thisIterTrain ).toDMVGrammar( posteriorMean = true )
      )

    if( printFinalGrammar )
      println( "Final grammar:\n" + estimator.g )
    else
      println( "Omitting final grammar for space considerations" )



    // println( "Parsing grammar:\n" + estimator.g.forNewSentences )


    println(
      "estimator.g.chooseScore(" + ChooseArgument( WordPair("IN","6"), RightAttachment ) + ", " +
      WordPair( "foo", "bar" ) + ") = " + math.exp( estimator.g.chooseScore(ChooseArgument( WordPair("IN","6"),
      RightAttachment ), WordPair( "foo", "bar" )) )
    )

    println(
      "estimator.g.chooseScore(" + ChooseArgument( WordPair("IN","4"), RightAttachment ) + ", " +
      WordPair( "foo", "bar" ) + ") = " + math.exp( estimator.g.chooseScore(ChooseArgument( WordPair("IN","4"),
      RightAttachment ), WordPair( "foo", "bar" )) )
    )

    println(
      "estimator.g.chooseScore(" + ChooseArgument( WordPair("blrgl","8"), RightAttachment ) + ", " +
      WordPair( "foo", "bar" ) + ") = " + math.exp( estimator.g.chooseScore(ChooseArgument( WordPair("blrgl","8"),
      RightAttachment ), WordPair( "foo", "bar" )) )
    )


    println(
      "estimator.g.stopScore(" + StopOrNot( WordPair("IN","4"), RightAttachment, true ) + ", " +
      Stop + ") = " + math.exp( estimator.g.stopScore( StopOrNot( WordPair("IN","4"),
      RightAttachment, true ), Stop ) )
    )

    println(
      "estimator.g.stopScore(" + StopOrNot( WordPair("IN","4"), RightAttachment, true ) + ", " +
      NotStop + ") = " + math.exp( estimator.g.stopScore( StopOrNot( WordPair("IN","4"),
      RightAttachment, true ), NotStop ) )
    )

    println(
      "estimator.g.stopScore(" + StopOrNot( WordPair("blrgl","8"), RightAttachment, true ) + ", " +
      Stop + ") = " + math.exp( estimator.g.stopScore( StopOrNot( WordPair("blrgl","8"),
      RightAttachment, true ), Stop ) )
    )

    println(
      "estimator.g.stopScore(" + StopOrNot( WordPair("blrgl","8"), RightAttachment, true ) + ", " +
      NotStop + ") = " + math.exp( estimator.g.stopScore( StopOrNot( WordPair("blrgl","8"),
      RightAttachment, true ), NotStop ) )
    )




    if( maxMarginalParse ) {
      println( estimator.maxMarginalParse(testSet, "convergence").mkString("\n", "\n", "\n"))
    } else if( annotateTestSet ) {
      println( estimator.annotatedMaxMarginalParse(testSet, "convergence").mkString("\n", "\n", "\n"))
    } else {
      val viterbiParser = new VanillaDMVParser { override val g = estimator.g/*.forNewSentences*/ }
      //viterbiParser.setGrammar( estimator.g )
      println( viterbiParser.bothParses(testSet, "convergence").mkString("\n", "\n", "\n"))
    }

    if( printArcProbabilities & printStopProbabilities ) {
      println( estimator.stopAndArcProbabilitiesCSV( testSet ).mkString("\n", "\n", "\n"))
    } else if( printArcProbabilities ) {
      println( estimator.arcProbabilitiesCSV( testSet ).mkString("\n", "\n", "\n"))
    } else if( printStopProbabilities ) {
      println( estimator.stopProbabilitiesCSV( testSet ).mkString("\n", "\n", "\n"))
    } else if( printSpanProbabilities ) {
      println( estimator.spanProbabilitiesCSV( testSet ).mkString("\n", "\n", "\n"))
    } else if( printFinalPartialCountsEachUtt ) {
      println( estimator.partialCountsCSV( testSet ).mkString("", "\n", "\n"))
    }
    // val viterbiParser = new VanillaDMVParser {
    //   override val g = new DMVTwoStreamStopGrammar
    // }
    // viterbiParser.setGrammar( estimator.g )
    // println( viterbiParser.bothParses(testSet, "convergence" ).mkString("\n", "\n", "\n"))
    // println( viterbiParser.dependencyParse( testSet ).mkString(
    //   "convergence:dependency:", "\nconvergence:dependency:", "" ) )
    // println( viterbiParser.constituencyParse( testSet ).mkString(
    //   "convergence:constituency:", "\nconvergence:constituency:", "" ) )

    pool.shutdown

  }

}


