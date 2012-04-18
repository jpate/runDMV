package runDMV.predictabilityParsers

import akka.actor.Actor
import joptsimple.OptionParser;
import joptsimple.OptionSet;
//import predictabilityParsing.util.CorpusManipulation
import predictabilityParsing.parsers.{VanillaDMVEstimator,VanillaDMVParser}
import predictabilityParsing.grammars.DMVBayesianBackoffThreeStreamGrammar
import predictabilityParsing.types.labels._

object DMVBayesianThreeStreamBackoff {
  def main( args:Array[String]) {

    val optsParser = new OptionParser()

    optsParser.accepts( "trainStrings" ).withRequiredArg
    optsParser.accepts( "testStrings" ).withRequiredArg
    optsParser.accepts( "grammarInit" ).withRequiredArg
    optsParser.accepts( "grammarInitMinLength" ).withRequiredArg
    optsParser.accepts( "rightFirst" ).withRequiredArg
    optsParser.accepts( "cAttach" ).withRequiredArg
    optsParser.accepts( "cStop" ).withRequiredArg
    optsParser.accepts( "cNotStop" ).withRequiredArg
    optsParser.accepts( "stopUniformity" ).withRequiredArg
    optsParser.accepts( "evalFreq" ).withRequiredArg
    optsParser.accepts( "unkCutoffA" ).withRequiredArg
    optsParser.accepts( "unkCutoffB" ).withRequiredArg
    optsParser.accepts( "unkCutoffC" ).withRequiredArg
    optsParser.accepts( "convergence" ).withRequiredArg
    optsParser.accepts( "minIter" ).withRequiredArg
    optsParser.accepts( "maxMarginalParse" )
    optsParser.accepts( "babySteps" )
    optsParser.accepts( "slidingBabySteps" )
    optsParser.accepts( "noBackoffAlpha" ).withRequiredArg
    optsParser.accepts( "backoffOneAlpha" ).withRequiredArg
    optsParser.accepts( "backoffTwoAlpha" ).withRequiredArg

    val opts = optsParser.parse( args:_* )

    val trainStrings = opts.valueOf( "trainStrings" ).toString
    val testStrings = opts.valueOf( "testStrings" ).toString

    val grammarInit =
      if(opts.has( "grammarInit" )) opts.valueOf( "grammarInit" ).toString else "hardlineStopHarmonicGrammar"

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

    val evalFreq =
      if(opts.has( "evalFreq" )) opts.valueOf( "evalFreq" ).toString.toInt else 4

    val unkCutoffA =
      if(opts.has( "unkCutoffA" )) opts.valueOf( "unkCutoffA" ).toString.toInt else 5

    val unkCutoffB =
      if(opts.has( "unkCutoffB" )) opts.valueOf( "unkCutoffB" ).toString.toInt else 5

    val unkCutoffC =
      if(opts.has( "unkCutoffC" )) opts.valueOf( "unkCutoffC" ).toString.toInt else 5

    val minIter =
      if(opts.has( "minIter" )) opts.valueOf( "minIter" ).toString.toDouble else 1D

    val convergence =
      if(opts.has( "convergence" )) opts.valueOf( "convergence").toString.toDouble else 0.00001

    val maxMarginalParse = opts.has( "maxMarginalParse" )

    val babySteps = if( opts.has( "babySteps" ) ) 0.00001D else 0.0D

    val slidingBabySteps = if( opts.has( "slidingBabySteps" ) ) 25 else 0

    var noBackoffAlpha =
      if( opts.has("noBackoffAlpha") ) opts.valueOf( "noBackoffAlpha").toString.toDouble
      else 35D

    var backoffOneAlpha =
      if( opts.has("backoffOneAlpha") ) opts.valueOf( "backoffOneAlpha").toString.toDouble
      else 52.5D

    var backoffTwoAlpha =
      if( opts.has("backoffTwoAlpha") ) opts.valueOf( "backoffTwoAlpha").toString.toDouble
      else 70D

    println( "trainStrings: " + trainStrings )
    println( "testStrings: " + testStrings )
    println( "grammarInit: " + grammarInit )
    println( "grammarInitMinLength: " + grammarInitMinLength )
    println( "rightFirst: " + rightFirst )
    println( "cAttach: " + cAttach )
    println( "cStop: " + cStop )
    println( "cNotStop: " + cNotStop )
    println( "stopUniformity: " + stopUniformity )
    println( "evalFreq: " + evalFreq )
    println( "unkCutoffA: " + unkCutoffA )
    println( "unkCutoffB: " + unkCutoffB )
    println( "unkCutoffC: " + unkCutoffC )
    println( "minIter: " + minIter )
    println( "convergence: " + convergence )
    println( "maxMarginalParse: " + maxMarginalParse )
    println( "babySteps: " + babySteps )
    println( "slidingBabySteps: " + slidingBabySteps )
    println( "noBackoffAlpha: " + noBackoffAlpha )
    println( "backoffOneAlpha: " + backoffOneAlpha )
    println( "backoffTwoAlpha: " + backoffTwoAlpha )


    print( "Reading in training set...." )
    val findRareWordsA = collection.mutable.Map[Word,Int]().withDefaultValue( 0 );
    val findRareWordsB = collection.mutable.Map[Word,Int]().withDefaultValue( 0 );
    val findRareWordsC = collection.mutable.Map[Word,Int]().withDefaultValue( 0 );
    var trainSet = io.Source.fromFile( trainStrings ).getLines.toList.map{ line =>
      val fields = line.split( " " ).toList
      ( (fields tail) zip (0 to ( fields.length-2 )) ).map{ case( s, t ) =>
        val wordParts = s.split( "[#_]" );

        val wt = WordTriple( wordParts(0), wordParts(1), wordParts(2) )

        //findRareWords( Word( wordParts(0) ) ) = 1 + findRareWords.getOrElse( Word( wordParts(0) ), 0 )
        findRareWordsA( Word( wordParts(0) ) ) = 1 + findRareWordsA.getOrElse( Word( wordParts(0) ), 0 )
        findRareWordsB( Word( wordParts(1) ) ) = 1 + findRareWordsB.getOrElse( Word( wordParts(1) ), 0 )
        findRareWordsC( Word( wordParts(2) ) ) = 1 + findRareWordsC.getOrElse( Word( wordParts(2) ), 0 )

        new TimedWordTriple( wordParts(0), wordParts(1), wordParts(2), t)
      }.toList
    }
    trainSet = trainSet.map( s =>
      s.map{ case TimedWordTriple( w1, w2, w3, t ) =>
        new TimedWordTriple(
          { if( findRareWordsA( Word( w1 ) ) <= unkCutoffA ) "UNKA" else w1 },
          { if( findRareWordsB( Word( w2 ) ) <= unkCutoffB ) "UNKB" else w2 },
          { if( findRareWordsC( Word( w3 ) ) <= unkCutoffC ) "UNKC" else w3 },
          t
        )
        // if( findRareWords( Word( w1 ) ) <= unkCutoff )
        //   new TimedWordTriple( "UNK", w2, w3, t )
        // else
        //   new TimedWordTriple( w1, w2, w3, t )
      }
    )
    println( " Done; " + trainSet.size + " training set strings" )

    print( "Reading in test set...." )
    val testSet = io.Source.fromFile( testStrings ).getLines.toList.map{ line =>
      val fields = line.split( " " ).toList
      TimedThreeStreamSentence(
        fields head,
        ( (fields tail) zip (0 to ( fields.length-2 )) ).map{ case( s,t ) =>

          val wordParts = s.split( "[#_]" );
          val wt = WordTriple( wordParts(0), wordParts(1), wordParts(2) )

          new TimedWordTriple(
            { if( findRareWordsA( Word( wordParts(0) ) ) <= unkCutoffA ) "UNKA" else wordParts(0) },
            { if( findRareWordsB( Word( wordParts(1) ) ) <= unkCutoffB ) "UNKB" else wordParts(1) },
            { if( findRareWordsC( Word( wordParts(2) ) ) <= unkCutoffC ) "UNKC" else wordParts(2) },
            t
          )


          // if( findRareWords.getOrElse( Word( wordParts(0) ), 0 )  <= unkCutoff ) {

          //   new TimedWordTriple( "UNK", wordParts(1), wordParts(2), t )

          // } else {
          //   new TimedWordTriple( wordParts(0), wordParts(1), wordParts(2), t)
          // }
        }.toList
      )
    }
    println( " Done; " + testSet.size + " test set strings" )

    val vocab = ( trainSet ++ testSet.map{ _.sentence } ).flatMap{ _.map{_.wt} }.toSet
    val streamBVocab = ( trainSet ++ testSet.map{ _.sentence } ).flatMap{ _.map{_.wt.obsB} }.toSet

    // if(
    //   stopNoBackoffAlpha == 0D &&
    //   stopBackoffAlpha == 0D &&
    //   chooseNoBackoffAlpha == 0D &&
    //   chooseBackoffHeadAlpha == 0D &&
    //   chooseBackoffArgAlpha == 0D &&
    //   chooseBackoffBothAlpha == 0D ) {

    //   println( "Calibrating alphas to vocab size" )

    //   stopNoBackoffAlpha = 1*streamBVocab.size ///10D
    //   stopBackoffAlpha = 2*streamBVocab.size ///10D

    //   chooseNoBackoffAlpha = 1*streamBVocab.size ///10D
    //   chooseBackoffArgAlpha = 1.5*streamBVocab.size ///10D
    //   chooseBackoffHeadAlpha = 1.5*streamBVocab.size ///10D
    //   chooseBackoffBothAlpha = 2*streamBVocab.size ///10D

    //   println( "stopNoBackoffAlpha: " + stopNoBackoffAlpha )
    //   println( "stopBackoffAlpha: " + stopBackoffAlpha )
    //   println( "chooseNoBackoffAlpha: " + chooseNoBackoffAlpha )
    //   println( "chooseBackoffHeadAlpha: " + chooseBackoffHeadAlpha )
    //   println( "chooseBackoffArgAlpha: " + chooseBackoffArgAlpha )
    //   println( "chooseBackoffBothAlpha: " + chooseBackoffBothAlpha )
    // }

    // This is the magic line... it should be enough to get this estimator relying on two stream
    // heads and stream A args...
    val estimator = new VanillaDMVEstimator {//( vocab )
      override val g = new DMVBayesianBackoffThreeStreamGrammar(
        noBackoffAlpha,
        backoffOneAlpha,
        backoffTwoAlpha
      )
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
      estimator.g.randomize(vocab)
    }
    println( " done" )

    // println( "Initial grammar:\n\n" + estimator.g )


    // val viterbiParser = new VanillaDMVParser
    // viterbiParser.setGrammar( estimator.g )

    /*(
    if( maxMarginalParse ) {
      // val viterbiParser = new VanillaDMVEstimator {
      //   override val g = estimator.g
      // }
      println( estimator.maxMarginalParse(testSet, "initial").mkString("\n", "\n", "\n"))
    } else {
      Actor.spawn{
        val viterbiParser = new VanillaDMVParser
        viterbiParser.setGrammar( estimator.g )
        println( viterbiParser.bothParses(testSet, "initial").mkString("\n", "\n", "\n"))
      }
    }
    */

    var deltaLogProb = 1D
    var lastCorpusLogProb = 1D
    var iter = 0

    var deltaFreeEnergy = 1D
    var lastGrammarFreeEnergy = 1D

    //var thisIterMaxSentLength = 3
    //var thisIterMaxSentLength = trainSet.map{ _.length}.sortWith( _ < _).head
    val longestSentence = trainSet.map{ _.length}.max
    val shortestSentenceLength = trainSet.map{ _.length}.min
    var slidingWindowLength:Int = 1
    var thisIterMaxSentLength = shortestSentenceLength

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
      val newPC = estimator.computePartialCounts( thisIterTrain )
      val corpusLogProb = newPC.getTotalScore
      deltaLogProb = ( ( lastCorpusLogProb - corpusLogProb ) / lastCorpusLogProb )

      println( "Iteration " + iter + ": " + corpusLogProb + " (" + deltaLogProb + ")" )

      val newGrammar =
        if( babySteps > 0 ) {
          println( "baby steps grammar" )
          newPC.toDMVGrammar
        } else if( slidingBabySteps > 0 ) {
          println( "sliding baby steps grammar" )
          newPC.toDMVGrammar
        } else {
          newPC.toDMVGrammar
        }

      //val newFreeEnergy = newGrammar.freeEnergy
      // deltaFreeEnergy = ( ( lastGrammarFreeEnergy - newFreeEnergy ) / lastGrammarFreeEnergy )
      // println( "Free Energy G_" + iter + ": " + newFreeEnergy + " (" + deltaFreeEnergy + ")" )

      // println( "New grammar:\n\n" + newGrammar )

      estimator.setGrammar( newGrammar )

      if( evalFreq != 0 && iter%evalFreq == 0 && babySteps == 0 && slidingBabySteps == 0) {
        val iterLabel = "it" + iter
        //Actor.spawn {
          if( maxMarginalParse ) {
            // val viterbiParser = new VanillaDMVEstimator {
            //   override val g = estimator.g
            // }
            println( estimator.maxMarginalParse(testSet, "it" + iter ).mkString("\n", "\n", "\n"))
          } else {
            Actor.spawn{
              // val viterbiParser = new VanillaDMVParser
              // viterbiParser.setGrammar( estimator.g )
              val viterbiParser = new VanillaDMVParser { override val g = estimator.g }
              println( viterbiParser.bothParses(testSet, "it" + iter ).mkString("\n", "\n", "\n"))
            }
          }
          // val viterbiParser = new VanillaDMVParser {
          //   override val g = new DMVTwoStreamStopGrammar
          // }
          // viterbiParser.setGrammar( newGrammar )
          // println( viterbiParser.bothParses(testSet, "it" + iter ).mkString("\n", "\n", "\n"))

          // println( viterbiParser.dependencyParse( testSet ).mkString(
          //   iterLabel+":dependency:", "\n"+iterLabel+":dependency:", "" ) )
          // println( viterbiParser.constituencyParse( testSet ).mkString(
          //   iterLabel+":constituency:", "\n"+iterLabel+":constituency:", "" ) )
        }
      //}

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

          //estimator.g.laplaceSmooth( thisIterTrain.flatMap{ _.toSet}.toSet, babySteps )
          //estimator.g.laplaceSmooth( 0.0001, thisIterTrain.flatten.map{_.w}.toSet )

          //println( "New Grammar:\n" + estimator.g )

          deltaLogProb = 1D
          lastCorpusLogProb = 1D
          // deltaFreeEnergy = 1D
          // lastGrammarFreeEnergy = 1D
        } else if( slidingBabySteps > 0 && slidingWindowLength < longestSentence ) {
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
            Actor.spawn {
              // val viterbiParser = new VanillaDMVParser
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
    }

    println( "Final grammar:\n" + estimator.g )


    if( maxMarginalParse ) {
      // val viterbiParser = new VanillaDMVEstimator {
      //   override val g = estimator.g
      // }
      println( estimator.maxMarginalParse(testSet, "convergence").mkString("\n", "\n", "\n"))
    } else {
      // val viterbiParser = new VanillaDMVParser
      // viterbiParser.setGrammar( estimator.g )
      val viterbiParser = new VanillaDMVParser { override val g = estimator.g }
      println( viterbiParser.bothParses(testSet, "convergence").mkString("\n", "\n", "\n"))
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

  }
}


