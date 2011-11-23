package runDMV.baselines

import akka.actor.Actor
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
    optsParser.accepts( "rightFirst" ).withRequiredArg
    optsParser.accepts( "cAttach" ).withRequiredArg
    optsParser.accepts( "cStop" ).withRequiredArg
    optsParser.accepts( "cNotStop" ).withRequiredArg
    optsParser.accepts( "stopUniformity" ).withRequiredArg
    // optsParser.accepts( "initialGrammar" ).withRequiredArg
    optsParser.accepts( "evalFreq" ).withRequiredArg
    optsParser.accepts( "unkCutoff" ).withRequiredArg

    val opts = optsParser.parse( args:_* )

    val trainStrings = opts.valueOf( "trainStrings" ).toString
    val testStrings = opts.valueOf( "testStrings" ).toString
    // val grammarInitialization =
    //   if(opts.has("initialGrammar")) opts.valueOf("initialGrammar").toString else "harmonic"

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

    val unkCutoff =
      if(opts.has( "unkCutoff" )) opts.valueOf( "unkCutoff" ).toString.toInt else 5


    println( "trainStrings: " + trainStrings )
    //println( "grammarInitialization: " + grammarInitialization )
    println( "testStrings: " + testStrings )
    println( "rightFirst: " + rightFirst )
    println( "cAttach: " + cAttach )
    println( "cStop: " + cStop )
    println( "cNotStop: " + cNotStop )
    println( "stopUniformity: " + stopUniformity )
    println( "evalFreq: " + evalFreq )
    println( "unkCutoff: " + unkCutoff )

    //val unkCutoff = 5

    print( "Reading in training set...." )
    val findRareWords = collection.mutable.Map[Word,Int]();
    var trainSet = io.Source.fromFile( trainStrings ).getLines.toList.map{ line =>
      val fields = line.split( " " ).toList
      ( (fields tail) zip (0 to ( fields.length-2 )) ).map{ case( s, t ) =>
        findRareWords( Word(s) ) = 1 + findRareWords.getOrElse( Word(s), 0 )
        new TimedWord(s,t)
      }.toList
    }
    trainSet = trainSet.map( s =>
      s.map{ case TimedWord( w, t ) =>
        if( findRareWords( Word( w ) ) <= unkCutoff )
          new TimedWord( "UNK", t )
        else
          new TimedWord( w, t )
      }
    )
    println( " Done; " + trainSet.size + " training set strings" )

    print( "Reading in test set...." )
    val testSet = io.Source.fromFile( testStrings ).getLines.toList.map{ line =>
      val fields = line.split( " " ).toList
      TimedSentence(
        fields head,
        ( (fields tail) zip (0 to ( fields.length-2 )) ).map{ case( s,t ) =>
          if( findRareWords.getOrElse( Word(s), 0 )  <= unkCutoff ) {
            println( "Considering " + s + " as UNK" )
            new TimedWord( "UNK", t )
          } else {
            new TimedWord(s,t)
          }
        }.toList
      )
    }
    println( " Done; " + testSet.size + " test set strings" )

    val vocab = ( trainSet ++ testSet.map{ _.sentence } ).flatMap{ _.map{_.w} }.toSet

    val estimator = new VanillaDMVEstimator

    //val initialGrammar =
    print( "Initializing harmonic grammar..." )
    estimator.setHarmonicGrammar(
      trainSet,
      rightFirst = rightFirst,
      cAttach = cAttach,
      cStop = cStop,
      cNotStop = cNotStop,
      stopUniformity = stopUniformity
    )

    println( " done" )

    println( "Initial grammar:\n\n" )
    println( estimator.g )


    // val viterbiParser = new VanillaDMVParser
    // viterbiParser.setGrammar( estimator.g )

    Actor.spawn{
      val viterbiParser = new VanillaDMVParser
      viterbiParser.setGrammar( estimator.g )
      println( viterbiParser.bothParses(testSet, "initial").mkString("\n", "\n", "\n"))
      // println(
      //   viterbiParser.dependencyParse( testSet ).mkString(
      //     "initial:dependency:", "\ninitial:dependency:", "\n" )
      // )
      // println(
      //   viterbiParser.constituencyParse( testSet ).mkString(
      //     "initial:constituency:", "\ninitial:constituency:", "\n" )
      // )
    }

    var deltaLogProb = 1D
    var lastCorpusLogProb = 1D
    var iter = 0

    println( "Beginning EM" )
    while( deltaLogProb > 0.00001 || deltaLogProb == (0D/0D) || iter < 10 ) {
      val newPC = estimator.computePartialCounts( trainSet )
      val corpusLogProb = newPC.getTotalScore
      deltaLogProb = ( ( lastCorpusLogProb - corpusLogProb ) / lastCorpusLogProb )

      println( "Iteration " + iter + ": " + corpusLogProb + " (" + deltaLogProb + ")" )

      val newGrammar = newPC.toDMVGrammar

      estimator.setGrammar( newGrammar )

      if( iter%evalFreq == 0 ) {
        val iterLabel = "it" + iter
        Actor.spawn {
          val viterbiParser = new VanillaDMVParser
          viterbiParser.setGrammar( newGrammar )
          println( viterbiParser.bothParses(testSet, "it" + iter ).mkString("\n", "\n", "\n"))

          // println( viterbiParser.dependencyParse( testSet ).mkString(
          //   iterLabel+":dependency:", "\n"+iterLabel+":dependency:", "" ) )
          // println( viterbiParser.constituencyParse( testSet ).mkString(
          //   iterLabel+":constituency:", "\n"+iterLabel+":constituency:", "" ) )
        }
      }
      iter += 1
      lastCorpusLogProb = corpusLogProb
    }

    println( "Final grammar:\n" + estimator.g )

    val viterbiParser = new VanillaDMVParser
    viterbiParser.setGrammar( estimator.g )
    println( viterbiParser.bothParses(testSet, "convergence" ).mkString("\n", "\n", "\n"))
    // println( viterbiParser.dependencyParse( testSet ).mkString(
    //   "convergence:dependency:", "\nconvergence:dependency:", "" ) )
    // println( viterbiParser.constituencyParse( testSet ).mkString(
    //   "convergence:constituency:", "\nconvergence:constituency:", "" ) )

  }
}


