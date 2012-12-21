package runDMV.baselines

//import akka.actor.Actor
import akka.dispatch.{ Future, ExecutionContext }
import java.util.concurrent.Executors
import joptsimple.OptionParser;
import joptsimple.OptionSet;
//import predictabilityParsing.util.CorpusManipulation
import predictabilityParsing.parsers.{VanillaDMVEstimator,VanillaDMVParser}
import predictabilityParsing.grammars.DMVGrammar
import predictabilityParsing.types.labels._

object VanillaDMV {
  def main( args:Array[String]) {

    val optsParser = new OptionParser()

    optsParser.accepts( "trainStrings" ).withRequiredArg
    optsParser.accepts( "testStrings" ).withRequiredArg
    optsParser.accepts( "grammarInit" ).withRequiredArg
    optsParser.accepts( "randomSeed" ).withRequiredArg
    optsParser.accepts( "grammarInitMinLength" ).withRequiredArg
    optsParser.accepts( "cAttach" ).withRequiredArg
    optsParser.accepts( "cStop" ).withRequiredArg
    optsParser.accepts( "cNotStop" ).withRequiredArg
    optsParser.accepts( "stopUniformity" ).withRequiredArg
    optsParser.accepts( "uniformRoot" )
    optsParser.accepts( "evalFreq" ).withRequiredArg
    optsParser.accepts( "unkCutoff" ).withRequiredArg
    optsParser.accepts( "vbEM" ).withRequiredArg
    optsParser.accepts( "stopAlpha" ).withRequiredArg
    optsParser.accepts( "chooseAlpha" ).withRequiredArg
    optsParser.accepts( "convergence" ).withRequiredArg
    optsParser.accepts( "minIter" ).withRequiredArg
    optsParser.accepts( "streamBBackoff" )
    optsParser.accepts( "maxMarginalParse" )
    optsParser.accepts( "babySteps" )
    optsParser.accepts( "slidingBabySteps" )
    optsParser.accepts( "printFinalGrammar" )
    optsParser.accepts( "randomSeed" ).withRequiredArg
    optsParser.accepts( "printFinalPartialCountsEachUtt" )
    optsParser.accepts( "printArcProbabilities" )
    optsParser.accepts( "printStopProbabilities" )

    val opts = optsParser.parse( args:_* )

    val trainStrings = opts.valueOf( "trainStrings" ).toString
    val testStrings = opts.valueOf( "testStrings" ).toString
    // val grammarInitialization =
    //   if(opts.has("initialGrammar")) opts.valueOf("initialGrammar").toString else "harmonic"

    val grammarInit =
      if(opts.has( "grammarInit" )) opts.valueOf( "grammarInit" ).toString else "hardlineStopHarmonicGrammar"

    val randomSeed =
      if(opts.has( "randomSeed" )) opts.valueOf( "randomSeed" ).toString.toDouble
      else 15D

    val grammarInitMinLength =
      if(opts.has( "grammarInitMinLength" )) opts.valueOf( "grammarInitMinLength" ).toString.toInt
      else 1

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

    val unkCutoff =
      if(opts.has( "unkCutoff" )) opts.valueOf( "unkCutoff" ).toString.toInt else 5

    val vbEM =
      if(opts.has( "vbEM" )) opts.valueOf( "vbEM" ).toString.toDouble else 0D

    var stopAlpha =
      if( opts.has("stopAlpha") ) opts.valueOf( "stopAlpha").toString.toDouble
      else vbEM

    var chooseAlpha =
      if( opts.has("chooseAlpha") ) opts.valueOf( "chooseAlpha").toString.toDouble
      else vbEM

    val minIter =
      if(opts.has( "minIter" )) opts.valueOf( "minIter" ).toString.toDouble else 1D

    val convergence =
      if(opts.has( "convergence" )) opts.valueOf( "convergence").toString.toDouble else 0.00001

    val streamBBackoff = opts.has( "streamBBackoff" )

    val maxMarginalParse = opts.has( "maxMarginalParse" )

    val babySteps = if( opts.has( "babySteps" ) ) 0.00001D else 0.0D

    val slidingBabySteps = if( opts.has( "slidingBabySteps" ) ) 25 else 0

    val printFinalGrammar = opts.has( "printFinalGrammar" )

    val printFinalPartialCountsEachUtt = opts.has( "printFinalPartialCountsEachUtt" )

    val printArcProbabilities = opts.has( "printArcProbabilities" )

    val printStopProbabilities = opts.has( "printStopProbabilities" )

    println( "trainStrings: " + trainStrings )
    println( "testStrings: " + testStrings )
    println( "grammarInit: " + grammarInit )
    println( "randomSeed: " + randomSeed )
    println( "grammarInitMinLength: " + grammarInitMinLength )
    println( "cAttach: " + cAttach )
    println( "cStop: " + cStop )
    println( "cNotStop: " + cNotStop )
    println( "stopUniformity: " + stopUniformity )
    println( "uniformRoot: " + uniformRoot )
    println( "evalFreq: " + evalFreq )
    println( "unkCutoff: " + unkCutoff )
    println( "vbEM: " + vbEM )
    println( "stopAlpha: " + stopAlpha )
    println( "chooseAlpha: " + chooseAlpha )
    println( "minIter: " + minIter )
    println( "convergence: " + convergence )
    println( "streamBBackoff: " + streamBBackoff )
    println( "maxMarginalParse: " + maxMarginalParse )
    println( "babySteps: " + babySteps )
    println( "slidingBabySteps: " + slidingBabySteps )
    println( "printFinalGrammar: " + printFinalGrammar )
    println( "printFinalPartialCountsEachUtt: " + printFinalPartialCountsEachUtt )
    println( "printArcProbabilities: " + printArcProbabilities )
    println( "printStopProbabilities: " + printStopProbabilities )

    //val unkCutoff = 5

    print( "Reading in training set...." )
    val findRareWords = collection.mutable.Map[ObservedLabel,Int]();
    var trainSet = io.Source.fromFile( trainStrings ).getLines.toList.map{ line =>
      val fields = line.split( " " ).toList
      ( (fields tail) zip (0 to ( fields.length-2 )) ).map{ case( s, t ) =>
        findRareWords( Word(s) ) = 1 + findRareWords.getOrElse( Word(s), 0 )
        new TimedWord(s,t)
      }.toList
    }
    trainSet = trainSet.map( s =>
        s.map{ case TimedWord( w, t ) =>
          if( ( findRareWords( Word( w ) ) <= unkCutoff ) && unkCutoff > 0 )
            if( streamBBackoff )
              new TimedWord( w.split("#")(1), t )
            else
              new TimedWord( "UNK", t )
          else
            new TimedWord( w, t )
        }
    )
    println( " Done; " + trainSet.size + " training set strings" )

    // println( "training set is:\n" )
    // println(
    //   trainSet.map(_.mkString(""," ","")).mkString("","\n","\n")
    // )
    // println( "\n\n" )

    print( "Reading in test set...." )
    val testSet = io.Source.fromFile( testStrings ).getLines.toList.map{ line =>
      val fields = line.split( " " ).toList
      TimedSentence(
        fields head,
        ( (fields tail) zip (0 to ( fields.length-2 )) ).map{ case( s,t ) =>
          if( ( findRareWords.getOrElse( Word(s), 0 )  <= unkCutoff ) && unkCutoff > 0 ) {
            if( streamBBackoff )
              new TimedWord( s.split("#")(1), t )
            else
              new TimedWord( "UNK", t )
          } else {
            new TimedWord(s,t)
          }
        }.toList
      )
    }
    println( " Done; " + testSet.size + " test set strings" )

    print( "training set: \n" + trainSet.map{_.mkString("", " ","")}.mkString("\n","\n","\n\n\n") )
    print( "test set: \n" + testSet.mkString("\n","\n","\n\n\n") )

    val vocab = ( trainSet ++ testSet.map{ _.sentence } ).flatMap{ _.map{_.w} }.toSet

    val estimator = new VanillaDMVEstimator


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
    } else if( grammarInit == "mismatchedHardlineStopHarmonicGrammar" ) {
      print( "Initializing mismatched hardline stop harmonic grammar..." )
      estimator.setMismatchedHardlineStopHarmonicGrammar(
        trainSet.filter{ _.length >= grammarInitMinLength },
        cAttach = cAttach,
        cStop = cStop,
        cNotStop = cNotStop,
        stopUniformity = stopUniformity
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

    // //val initialGrammar =
    // print( "Initializing harmonic grammar..." )
    // estimator.setHarmonicGrammar(
    //   trainSet,
    //   rightFirst = rightFirst,
    //   cAttach = cAttach,
    //   cStop = cStop,
    //   cNotStop = cNotStop,
    //   stopUniformity = stopUniformity
    // )

    println( " done" )

    //println( "Initial grammar:\n\n" + estimator.g )


    // val viterbiParser = new VanillaDMVParser
    // viterbiParser.setGrammar( estimator.g )

    // Actor.spawn{
    //   if( maxMarginalParse ) {
    //     val viterbiParser = new VanillaDMVEstimator
    //     viterbiParser.setGrammar( estimator.g )
    //     println( viterbiParser.maxMarginalParse(testSet, "initial").mkString("\n", "\n", "\n"))
    //   } else {
    //     val viterbiParser = new VanillaDMVParser
    //     viterbiParser.setGrammar( estimator.g )
    //     println( viterbiParser.bothParses(testSet, "initial").mkString("\n", "\n", "\n"))
    //   }
    //   // println(
    //   //   viterbiParser.dependencyParse( testSet ).mkString(
    //   //     "initial:dependency:", "\ninitial:dependency:", "\n" )
    //   // )
    //   // println(
    //   //   viterbiParser.constituencyParse( testSet ).mkString(
    //   //     "initial:constituency:", "\ninitial:constituency:", "\n" )
    //   // )
    // }

    var deltaLogProb = 1D
    var lastCorpusLogProb = 1D
    var iter = 0

    var thisIterMaxSentLength = 3
    val longestSentence = trainSet.map{ _.length}.sortWith( _ > _).head
    var slidingWindowLength:Int = 1

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


    val pool = Executors.newCachedThreadPool()
    implicit val ec = ExecutionContext.fromExecutorService( pool )
    println( "Beginning EM" )
    //while( deltaLogProb > 0.00001 || deltaLogProb == (0D/0D) || iter < 10 ) {
    while(
      math.abs( deltaLogProb ) > convergence ||
      deltaLogProb == (0D/0D) ||
      iter < minIter
    ) {
      //val newPC = estimator.computePartialCounts( trainSet )
      val newPC = estimator.computePartialCounts( thisIterTrain )
      val corpusLogProb = newPC.getTotalScore
      deltaLogProb = ( ( lastCorpusLogProb - corpusLogProb ) / lastCorpusLogProb )

      println( "Iteration " + iter + ": " + corpusLogProb + " (" + deltaLogProb + ")" )

      //val newGrammar = newPC.toDMVGrammar
      val newGrammar =
        if( vbEM > 0 ) {
          println( "VB grammar" )
          newPC.toVariationalDMVGrammar( stopAlpha = stopAlpha, chooseAlpha = chooseAlpha )
        } else if( babySteps > 0 ) {
          println( "Baby steps grammar" )
          newPC.toLaplaceSmoothedGrammar( vocab, babySteps )
        } else if( slidingBabySteps > 0 ) {
          println( "sliding baby steps grammar" )
          //newPC.toLaplaceSmoothedGrammar( vocab, 0.00001D )
          newPC.toDMVGrammar
        } else {
          println( "MLE grammar" )
          newPC.toDMVGrammar
        }
      // println( "newGrammar:\n" + newGrammar )

      estimator.setGrammar( newGrammar )

      if( evalFreq != 0 && iter%evalFreq == 0 && babySteps == 0 && slidingBabySteps == 0) {
        val iterLabel = "it" + iter
        Future {
          if( maxMarginalParse ) {
            val viterbiParser = new VanillaDMVEstimator
            viterbiParser.setGrammar( estimator.g )
            println( viterbiParser.maxMarginalParse(testSet, "it" + iter ).mkString("\n", "\n", "\n"))
          } else {
            //val viterbiParser = new VanillaDMVParser( randomSeed )
            val viterbiParser = new VanillaDMVParser { override val g = estimator.g }
            //viterbiParser.setGrammar( estimator.g )
            println( viterbiParser.bothParses(testSet, "it" + iter ).mkString("\n", "\n", "\n"))
          }
        }
      }
      iter += 1
      lastCorpusLogProb = corpusLogProb

      if(
        math.abs( deltaLogProb ) <= convergence &&
        thisIterMaxSentLength < longestSentence
      ) {
        if( babySteps > 0D ) {
          println(
            "Jumping from maxlength of " + thisIterMaxSentLength + " to " + (thisIterMaxSentLength+1)
          )
          thisIterMaxSentLength += 1
          thisIterTrain = trainSet.filter{ _.length <= thisIterMaxSentLength }

          val iterLabel = "sentence"+slidingWindowLength+"Converged"
          Future {
            if( maxMarginalParse ) {
              val viterbiParser = new VanillaDMVEstimator
              viterbiParser.setGrammar( estimator.g )
              println( viterbiParser.maxMarginalParse(testSet, "it" + iter ).mkString("\n", "\n", "\n"))
            } else {
              // val viterbiParser = new VanillaDMVParser( randomSeed )
              // viterbiParser.setGrammar( estimator.g )
              val viterbiParser = new VanillaDMVParser { override val g = estimator.g }
              println( viterbiParser.bothParses(testSet, "it" + iter ).mkString("\n", "\n", "\n"))
            }
          }

          // estimator.g.laplaceSmooth( thisIterTrain.flatMap{ _.toSet}.toSet, babySteps )

          //println( "New Grammar:\n" + estimator.g )

          deltaLogProb = 1D
          lastCorpusLogProb = 1D
        } else if( slidingBabySteps > 0 && slidingWindowLength < longestSentence) {
          println(
            "Jumping from sliding window size of " + slidingWindowLength +
              " to " + (slidingWindowLength+1)
          )

          val iterLabel = "window"+slidingWindowLength+"Converged"
          if( slidingWindowLength % evalFreq == 0 )
            Future {
              if( maxMarginalParse ) {
                val viterbiParser = new VanillaDMVEstimator
                viterbiParser.setGrammar( estimator.g )
                println( viterbiParser.maxMarginalParse(testSet, iterLabel ).mkString("\n", "\n", "\n"))
              } else {
                // val viterbiParser = new VanillaDMVParser( randomSeed )
                // viterbiParser.setGrammar( estimator.g )
                val viterbiParser = new VanillaDMVParser { override val g = estimator.g }
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

          estimator.g.laplaceSmooth( 0.0001, thisIterTrain.flatten.map{_.w}.toSet )

          //println( estimator.g.stopScore( estimator.g.p_stop.parents.head , Stop) )
          // println( "\n\n\nAFTER SMOOTHING\n\n\n" )
          // println( estimator.g )
          // println( estimator.g.stopScore( estimator.g.p_stop.parents.head , Stop) )

          // println( thisIterTrain.flatten.toSet.mkString( "\t\t","\n\t\t","\n\n-----\n\n" ) )

          deltaLogProb = 1D
          lastCorpusLogProb = 1D
        }
      }
    }

    if( printFinalGrammar )
      println( "Final grammar:\n" + estimator.g )
    else
      println( "Omitting final grammar for space considerations" )


    println(
      "estimator.g.chooseScore(" + ChooseArgument( Word("IN"), RightAttachment ) + ", " +
      Word( "foo" ) + ") = " + math.exp( estimator.g.chooseScore(ChooseArgument( Word("IN"),
      RightAttachment ), Word( "foo" )) )
    )

    println(
      "estimator.g.chooseScore(" + ChooseArgument( Word("IN"), RightAttachment ) + ", " +
      Word( "foo" ) + ") = " + math.exp( estimator.g.chooseScore(ChooseArgument( Word("IN"),
      RightAttachment ), Word( "foo" )) )
    )

    println(
      "estimator.g.chooseScore(" + ChooseArgument( Word("blrgl"), RightAttachment ) + ", " +
      Word( "foo" ) + ") = " + math.exp( estimator.g.chooseScore(ChooseArgument( Word("blrgl"),
      RightAttachment ), Word( "foo" ) ))
    )

    println(
      "estimator.g.stopScore(" + StopOrNot( Word("IN"), RightAttachment, true ) + ", " +
      Stop + ") = " + math.exp( estimator.g.stopScore( StopOrNot( Word("IN"),
      RightAttachment, true ), Stop ) )
    )

    println(
      "estimator.g.stopScore(" + StopOrNot( Word("IN"), RightAttachment, true ) + ", " +
      NotStop + ") = " + math.exp( estimator.g.stopScore( StopOrNot( Word("IN"),
      RightAttachment, true ), NotStop ) )
    )

    println(
      "estimator.g.stopScore(" + StopOrNot( Word("blrgl"), RightAttachment, true ) + ", " +
      Stop + ") = " + math.exp( estimator.g.stopScore( StopOrNot( Word("blrgl"),
      RightAttachment, true ), Stop ) )
    )

    println(
      "estimator.g.stopScore(" + StopOrNot( Word("blrgl"), RightAttachment, true ) + ", " +
      NotStop + ") = " + math.exp( estimator.g.stopScore( StopOrNot( Word("blrgl"),
      RightAttachment, true ), NotStop ) )
    )


    println(
      "estimator.g.stopScore(" + StopOrNot( Word("UNK"), RightAttachment, true ) + ", " +
      Stop + ") = " + math.exp( estimator.g.stopScore( StopOrNot( Word("UNK"),
      RightAttachment, true ), Stop ) )
    )

    println(
      "estimator.g.stopScore(" + StopOrNot( Word("UNK"), RightAttachment, true ) + ", " +
      NotStop + ") = " + math.exp( estimator.g.stopScore( StopOrNot( Word("UNK"),
      RightAttachment, true ), NotStop ) )
    )



    // println(
    //   "estimator.g.chooseScore(" + ChooseArgument( Word("IN"), RightAttachment ) + ", " +
    //   Word( "foo" ) + ") = " + estimator.g.chooseScore(ChooseArgument( Word("IN"),
    //   RightAttachment ), Word( "foo" ))
    // )

    // println(
    //   "estimator.g.chooseScore(" + ChooseArgument( Word("bar"), RightAttachment ) + ", " +
    //   Word( "foo" ) + ") = " + estimator.g.chooseScore(ChooseArgument( Word("bar"),
    //   RightAttachment ), Word( "foo" ))
    // )





    // val viterbiParser = new VanillaDMVParser
    // viterbiParser.setGrammar( estimator.g )
    // println( viterbiParser.bothParses(testSet, "convergence" ).mkString("\n", "\n", "\n"))

    if( maxMarginalParse ) {
      val viterbiParser = new VanillaDMVEstimator
      viterbiParser.setGrammar( estimator.g )
      println( viterbiParser.maxMarginalParse(testSet, "convergence").mkString("\n", "\n", "\n"))
    } else if( printArcProbabilities & printStopProbabilities ) {
      println( estimator.stopAndArcProbabilitiesCSV( testSet ).mkString("\n", "\n", "\n"))
    } else if( printArcProbabilities ) {
      println( estimator.arcProbabilitiesCSV( testSet ).mkString("\n", "\n", "\n"))
    } else if( printStopProbabilities ) {
      println( estimator.stopProbabilitiesCSV( testSet ).mkString("\n", "\n", "\n"))
    } else if( printFinalPartialCountsEachUtt ) {
      println( estimator.partialCountsCSV( testSet ).mkString("", "\n", "\n"))
    } else {
      // val viterbiParser = new VanillaDMVParser( randomSeed )
      // viterbiParser.setGrammar( estimator.g )
      val viterbiParser = new VanillaDMVParser { override val g = estimator.g }
      println( viterbiParser.bothParses(testSet, "convergence").mkString("\n", "\n", "\n"))
    }

    // println( viterbiParser.dependencyParse( testSet ).mkString(
    //   "convergence:dependency:", "\nconvergence:dependency:", "" ) )
    // println( viterbiParser.constituencyParse( testSet ).mkString(
    //   "convergence:constituency:", "\nconvergence:constituency:", "" ) )

    pool.shutdown

  }
}


