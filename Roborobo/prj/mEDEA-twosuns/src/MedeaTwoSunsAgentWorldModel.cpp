/*
 *  MedeaTwoSunsAgentWorldModel.cpp
 *  roborobo-online
 *
 *  Created by Nicolas on 15/04/10.
 *  Copyright 2009 __MyCompanyName__. All rights reserved.
 *
 */

#include "mEDEA-twosuns/include/MedeaTwoSunsAgentWorldModel.h"

MedeaTwoSunsAgentWorldModel::MedeaTwoSunsAgentWorldModel()
{
}

MedeaTwoSunsAgentWorldModel::~MedeaTwoSunsAgentWorldModel()
{
}

double MedeaTwoSunsAgentWorldModel::getEnergyPointDirectionAngleValue()
{
	return _energyPointDirectionAngleValue;
}

double MedeaTwoSunsAgentWorldModel::getEnergyPointDistanceValue()
{
	return _energyPointDistanceValue;
}

void MedeaTwoSunsAgentWorldModel::setEnergyPointDirectionAngleValue( double __value )
{
	_energyPointDirectionAngleValue = __value;
}

void MedeaTwoSunsAgentWorldModel::setEnergyPointDistanceValue( double __value )
{
	_energyPointDistanceValue = __value;
}

bool MedeaTwoSunsAgentWorldModel::getActiveStatus()
{
	return _isActive;
}

void MedeaTwoSunsAgentWorldModel::setActiveStatus( bool __isActive )
{
	_isActive = __isActive;
}

double MedeaTwoSunsAgentWorldModel::getEnergyLevel()
{
	return _energyLevel;
}

void MedeaTwoSunsAgentWorldModel::setEnergyLevel(double inValue)
{
	_energyLevel = inValue;
}

double MedeaTwoSunsAgentWorldModel::getDeltaEnergy()
{
	return _deltaEnergy;
}

void MedeaTwoSunsAgentWorldModel::setDeltaEnergy(double inValue)
{
	_deltaEnergy = inValue;
}

void MedeaTwoSunsAgentWorldModel::setAngleToClosestEnergyPoint( double __value )
{
	_angleToClosestEnergyPoint = __value;
}

double MedeaTwoSunsAgentWorldModel::getAngleToClosestEnergyPoint()
{
	return _angleToClosestEnergyPoint;
}
		
void MedeaTwoSunsAgentWorldModel::setDistanceToClosestEnergyPoint( double __value )
{
	_distanceToClosestEnergyPoint = __value;
}

double MedeaTwoSunsAgentWorldModel::getDistanceToClosestEnergyPoint()
{
	return _distanceToClosestEnergyPoint;
}


void MedeaTwoSunsAgentWorldModel::resetActiveGenome()
{
	int nbInput = _sensorCount + 5 ; //sensors + floorSensor + energyDirection + energyLevel + energyDistance  +bias
	int nbOutput = 2 ; // rotation + velocity 
	int nbGene = ( nbInput * _nbHiddenNeurons ) + ( _nbHiddenNeurons * nbOutput) + nbOutput ; // from input to hidden layer + from hiddenLayer to output + from bias to output
	std::cout << std::flush ;
	_genome.clear();
	for ( int i = 0 ; i != nbGene ; i++ )
	{
		_genome.push_back(((rand()%800)/400.0)-1.0);
	}
	_currentGenome = _genome;
	setNewGenomeStatus(true);
	_genomesList.clear();
}
