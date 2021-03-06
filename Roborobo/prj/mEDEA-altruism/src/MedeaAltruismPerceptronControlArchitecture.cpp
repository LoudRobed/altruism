/*
 *  MedeaAltruismPerceptronControlArchitecture.cpp
 *  roborobo-online
 *
 *  imported from Jean-Marc on 21/12/09
 *  current dev: Nicolas on 1/4/2009
 *  moved in altruism by Jean-Marc on 17/09/2010
 *
 */

#include "mEDEA-altruism/include/MedeaAltruismPerceptronControlArchitecture.h"

#include "World/World.h"

#include "Utilities/Misc.h"

#include <math.h>

MedeaAltruismPerceptronControlArchitecture::MedeaAltruismPerceptronControlArchitecture( RobotAgentWorldModel *__wm )
{
	_wm = (MedeaAltruismAgentWorldModel*)__wm;
	_iteration = 0;

	//_initialEnergy = 2*MedeaAltruismSharedData::gEvaluationTime*0.1;  // original CEC
	//_initialEnergy = 2*MedeaAltruismSharedData::gEvaluationTime;  // original CEC, adapte avec nv schema
	//_initialEnergy = MedeaAltruismSharedData::gEvaluationTime*0.1 / 2; // half the maximum lifetime.  // used to be: 2*MedeaAltruismSharedData::gEvaluationTime*0.1;
	//_initialEnergy = MedeaAltruismSharedData::gEvaluationTime*2; // MUST BE AT LEAST MedeaAltruismSharedData::gEvaluationTime otw a dead agent will never be revived  // / 2.0; // half the maximum lifetime + nv schema (entier)
	
	_wm->setEnergyLevel(MedeaAltruismSharedData::gEnergyInit);
	//_deltaEnergy = 10.0; // CEC
	_wm->setDeltaEnergy(0.0); // MedeaAltruism

	_wm->setAngleToClosestEnergyPoint(0);
	_wm->setDistanceToClosestEnergyPoint(0);

	gProperties.checkAndGetPropertyValue("nbHiddenNeurons",&_nbHiddenNeurons,true);
	
	_wm->setActiveStatus(true);

	if ( gVerbose )
	{
		std::cout << "robot #" << _wm->_agentId << " perceptron \n" ;	
	}
}

MedeaAltruismPerceptronControlArchitecture::~MedeaAltruismPerceptronControlArchitecture()
{
	// nothing to do.
}

void MedeaAltruismPerceptronControlArchitecture::reset()
{
	_parameters.clear();
	_parameters = _wm->_currentGenome;
}


// perform one controller update
// set motor value using motor format.
void MedeaAltruismPerceptronControlArchitecture::step()
{
	_iteration++;

	if ( _wm->getNewGenomeStatus() ) // check for new NN parameters
	{
		reset();
		_wm->setNewGenomeStatus(false);
	}

/*	if ( _wm->_age < 0 ) // problem: _age is nowhere to be incremented
	{
		// ** security control (prior to a new behavior, get out of crash situation) -- random noise to avoid "STALL" status
		_wm->_desiredTranslationalValue = ( ranf()*2.-1. ) * gMaxTranslationalSpeed ;
		_wm->_desiredRotationalVelocity =( ranf()*2.-1. ) * gMaxRotationalSpeed ;
		currentObserver->setKey( ( ranf()*2.-1. ) * MedeaAltruismSharedData::gMaxKeyRange);
		return;
	}
*/
	_wm->_desiredTranslationalValue = 0.0;
	_wm->_desiredRotationalVelocity = 0.0;

	if ( _wm->getActiveStatus() == true )
	{
		double angleToClosestEnergyPoint = 0.0;
		double shortestDistance = 0.0;
		//search the closest energy point
		Point2d posRobot(_wm->_xReal,_wm->_yReal);
		std::vector<EnergyPoint>::iterator closestPoint = gEnergyPoints.begin();
		shortestDistance = getEuclidianDistance (posRobot,closestPoint->getPosition());

		for(std::vector<EnergyPoint>::iterator it = gEnergyPoints.begin()+1; it < gEnergyPoints.end(); it++)
		{
			if (it->getActiveStatus() == true)
			{
				double newDistance = getEuclidianDistance (posRobot,it->getPosition());
				if(newDistance < shortestDistance)
				{
					shortestDistance = newDistance;
					closestPoint = it;
				}
			}
		}
		//compute the orientation of the closest energy point ( in degree between 0 and 360 )
		angleToClosestEnergyPoint = (atan2(closestPoint->getPosition().y-posRobot.y,closestPoint->getPosition().x-posRobot.x)/M_PI)*180.0;
		angleToClosestEnergyPoint += 360.0 ;
		angleToClosestEnergyPoint = computeModulo(angleToClosestEnergyPoint,360.0);
		if ( angleToClosestEnergyPoint > 180 ) // force btw -180 and 180
			angleToClosestEnergyPoint -= 360.0;

		//compute the angle between the actual orientation of the robot and the orientation of the closest energy point ( in degree between -180 and 180 )
		double diffAngleToClosestEnergyPoint = angleToClosestEnergyPoint -  _wm->_agentAbsoluteOrientation ;
		if ( diffAngleToClosestEnergyPoint < -180.0 )
		{
			diffAngleToClosestEnergyPoint += 360.0 ; 
		}
		if ( diffAngleToClosestEnergyPoint > 180.0 )
		{
			diffAngleToClosestEnergyPoint -= 360.0 ;
		}
		
		//cast the diffAngle between -1 and 1
		diffAngleToClosestEnergyPoint = diffAngleToClosestEnergyPoint / 180.0 ; 
		_wm->setEnergyPointDirectionAngleValue(diffAngleToClosestEnergyPoint);
		
		//cast the shortest distance between 0 and 1
		//comparer avec le capteur de distance, normalisation
		//shortestDistance = shortestDistance / sqrt(pow(gAreaWidth,2)+pow(gAreaHeight,2));
		//_wm->setEnergyPointDistanceValue(shortestDistance);

		// !n - lu11h53 - info marker (delete comment after 20100420)
		if ( shortestDistance > gSensorRange )
			shortestDistance = 1.0;
		else
			shortestDistance = shortestDistance / (double)gSensorRange;
		_wm->setEnergyPointDistanceValue(shortestDistance);

		if ( gVerbose && gInspectAgent && gAgentIndexFocus == _wm->_agentId )
		{
			std::cout << "tgt.in: " << _wm->getEnergyPointDirectionAngleValue() << std::endl;// " (" << _wm->_agentAbsoluteOrientation << "," << angleToClosestEnergyPoint << ")" << std::endl;
		}

		std::vector<double> hiddenLayer;
		hiddenLayer.resize(_nbHiddenNeurons);
		for (int j= 0 ; j < _nbHiddenNeurons ; j++ )
		{
			hiddenLayer[j] = 0.0;
		}

		int geneToUse = 0;

		// inputs to hidden Layer

		// distance sensors
		for ( int i = 0 ; i < _wm->_sensorCount ; i++ )
		{
			for (int j= 0 ; j < _nbHiddenNeurons ; j++ )
			{
				// BUG: hiddenLayer[j] += tanh(_wm->_sensorCount) * _parameters[geneToUse] ;  // ??? !N - BUG di0319 !n -- sensor are NOT taken into account!
				hiddenLayer[j] += (_wm->getSensorDistanceValue(i)/_wm->getSensorMaximumDistanceValue(i)) * _parameters[geneToUse] ;  // !N - corrected BUG di0319 - use normalized sensor value in 0...1
				geneToUse ++;
			}
		}

		//floor sensor
		for (int j= 0 ; j < _nbHiddenNeurons ; j++ )
		{
			if ( _wm->_floorSensor != 0 )		// binary detector -- either something, or nothing.
				hiddenLayer[j] += 1.0  * _parameters[geneToUse];
			//hiddenLayer[j] += _wm->_floorSensor/255.0  * _parameters[geneToUse];
			geneToUse ++;
		}

		//direction of the closest energy point
		for (int j= 0 ; j < _nbHiddenNeurons ; j++ )
		{
			hiddenLayer[j] += _wm->getEnergyPointDirectionAngleValue()  * _parameters[geneToUse];		// ??? !N - should be angle. naming problem, code correct.
			geneToUse ++;
		}

		//distance to the closest energy point
		for (int j= 0 ; j < _nbHiddenNeurons ; j++ )
		{
			hiddenLayer[j] += _wm->getEnergyPointDistanceValue()  * _parameters[geneToUse];  // ??? !N - should be reference to _wm (convention with previous)
			geneToUse ++;
		}

		//energy level
		for (int j= 0 ; j < _nbHiddenNeurons ; j++ )
		{
			hiddenLayer[j] +=  (_wm->getEnergyLevel()/MedeaAltruismSharedData::gEnergyMax)  * _parameters[geneToUse];  // !N : added: energy value normalization btw 0 and 1.
			geneToUse ++;
		}

		//bias
		for (int j= 0 ; j < _nbHiddenNeurons ; j++ )
		{
			hiddenLayer[j] += 1.0  * _parameters[geneToUse];
			geneToUse ++;
		}

		//activation function on hidden layer
		for (int j= 0 ; j < _nbHiddenNeurons ; j++ )
		{
			hiddenLayer[j] = tanh(hiddenLayer[j]);
		}
		
		//hiddenLayer to output
		_wm->_desiredTranslationalValue = 0;
		_wm->_desiredRotationalVelocity = 0;
		double energyHarvestingRate = 0.0;

		for (int j= 0 ; j < _nbHiddenNeurons ; j++ )
		{
			_wm->_desiredTranslationalValue += hiddenLayer[j] * _parameters[geneToUse] ;
			geneToUse ++;
		}
		for (int j= 0 ; j < _nbHiddenNeurons ; j++ )
		{
			_wm->_desiredRotationalVelocity += hiddenLayer[j] * _parameters[geneToUse] ;
			geneToUse ++;
		}
		for (int j= 0 ; j < _nbHiddenNeurons ; j++ )
		{
			energyHarvestingRate += hiddenLayer[j] * _parameters[geneToUse] ;
			geneToUse ++;
		}
		
		_wm->_desiredTranslationalValue += 1.0 * _parameters[geneToUse] ;
		geneToUse ++;
		_wm->_desiredRotationalVelocity += 1.0 * _parameters[geneToUse] ;
		geneToUse ++;
		energyHarvestingRate += 1.0 * _parameters[geneToUse] ;

		//activation function on output
		_wm->_desiredTranslationalValue = tanh( _wm->_desiredTranslationalValue ) ;  // !N note that tanh is optional for ANN outputs.
		_wm->_desiredRotationalVelocity = tanh( _wm->_desiredRotationalVelocity );
		energyHarvestingRate = tanh( energyHarvestingRate );
		energyHarvestingRate = (energyHarvestingRate+1.0)/2.0;

		// normalize to motor interval values
		_wm->_desiredTranslationalValue = _wm->_desiredTranslationalValue * gMaxTranslationalSpeed;
		_wm->_desiredRotationalVelocity = _wm->_desiredRotationalVelocity * gMaxRotationalSpeed;

		//apply the output value
		_wm->setEnergyHarvestingRate(energyHarvestingRate);
		_wm->setEnergyDonation(energyHarvestingRate);

	}

}

