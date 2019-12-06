--{-# LANGUAGE OverloadedStrings #-}

module Day6Tests where

import Day6

import TestHelpers

import Hedgehog
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog
