#pragma once

#include "expression.h"

#include <cmath>
#include <functional>
#include <limits>

template <class Number>
struct BaseExpressionTraits
{
protected:
    BaseExpressionTraits() = delete;

    template <class T = int64_t>
    static constexpr bool checkBounds(double n)
    {
        return n >= std::nextafter(std::numeric_limits<T>::min(), 0) && n <= std::nextafter(std::numeric_limits<T>::max(), 0);
    }

    template <class T>
    static constexpr bool checkBounds(T n) = delete;

public:
    struct UnaryOperation
    {
    private:
        const std::function<Number(const Number &)> forNumbers;
        const std::function<Expression *(const Expression &)> forExpressions;

        static bool noCheck(const Number &)
        {
            return true;
        }

    public:
        const std::function<bool(const Number &)> checkArgs;

        template <class T>
        UnaryOperation(
                std::function<Number(const Number &)> const & forNumbers,
                T (*forExpressions)(const Expression &),
                std::function<bool(const Number &)> const & checkArgs = noCheck)
            : forNumbers(forNumbers)
            , forExpressions([=](const Expression & arg) {
                return forExpressions(arg).clone();
            })
            , checkArgs(checkArgs)
        {
        }

        Number operator()(const Number & arg) const
        {
            return forNumbers(arg);
        }

        Expression * operator()(const Expression & arg) const
        {
            return forExpressions(arg);
        }
    };

    struct BinaryOperation
    {
    private:
        const std::function<Number(const Number &, const Number &)> forNumbers;
        const std::function<Expression *(const Expression &, const Expression &)> forExpressions;

        static bool noCheck(const Number &, const Number &)
        {
            return true;
        }

    public:
        const std::function<bool(const Number &, const Number &)> checkArgs;

        template <class T>
        BinaryOperation(
                std::function<Number(const Number &, const Number &)> const & forNumbers,
                T (*forExpressions)(const Expression &, const Expression &),
                std::function<bool(const Number &, const Number &)> const & checkArgs = noCheck)
            : forNumbers(forNumbers)
            , forExpressions([=](const Expression & left, const Expression & right) {
                return forExpressions(left, right).clone();
            })
            , checkArgs(checkArgs)
        {
        }

        Number operator()(const Number & left, const Number & right) const
        {
            return forNumbers(left, right);
        }

        Expression * operator()(const Expression & left, const Expression & right) const
        {
            return forExpressions(left, right);
        }
    };
};

template <class Number>
struct ExpressionTraits;

#include "expression_traits.tpp"
