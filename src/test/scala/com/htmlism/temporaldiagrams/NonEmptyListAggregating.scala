package com.htmlism.temporaldiagrams

import cats.data.NonEmptyList
import org.scalatest.enablers.Aggregating

trait NonEmptyListAggregating {
  implicit def nonEmptyListAggregating[A](implicit agg: Aggregating[List[A]]): Aggregating[NonEmptyList[A]] =
    new Aggregating[NonEmptyList[A]] {
      def containsAtLeastOneOf(aggregation: NonEmptyList[A], eles: collection.Seq[Any]): Boolean =
        agg.containsAtLeastOneOf(aggregation.toList, eles)

      def containsTheSameElementsAs(leftAggregation: NonEmptyList[A], rightAggregation: Iterable[Any]): Boolean =
        agg.containsTheSameElementsAs(leftAggregation.toList, rightAggregation)

      def containsOnly(aggregation: NonEmptyList[A], eles: collection.Seq[Any]): Boolean =
        agg.containsOnly(aggregation.toList, eles)

      def containsAllOf(aggregation: NonEmptyList[A], eles: collection.Seq[Any]): Boolean =
        agg.containsAllOf(aggregation.toList, eles)

      def containsAtMostOneOf(aggregation: NonEmptyList[A], eles: collection.Seq[Any]): Boolean =
        agg.containsAtMostOneOf(aggregation.toList, eles)
    }
}
