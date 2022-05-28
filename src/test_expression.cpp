#include "expression.h"
#include "expression_traits.h"

#include <algorithm>
#include <gtest/gtest.h>
#include <iterator>
#include <memory>
#include <random>
#include <set>
#include <sstream>
#include <tuple>
#include <type_traits>

template <std::size_t Operands>
static constexpr std::size_t MAX_SIZE = sizeof(void *) * (2 + Operands * 2);

TEST(ExpressionTest, traits)
{
    EXPECT_LE(sizeof(Negate), MAX_SIZE<1>);
    EXPECT_LE(sizeof(Add), MAX_SIZE<2>);
    EXPECT_LE(sizeof(Subtract), MAX_SIZE<2>);
    EXPECT_LE(sizeof(Multiply), MAX_SIZE<2>);
    EXPECT_LE(sizeof(Divide), MAX_SIZE<2>);

    EXPECT_FALSE(std::is_default_constructible_v<Expression>);
}

TEST(ExpressionTest, constant)
{
    EXPECT_EQ(Const(0).eval(), 0);
    EXPECT_EQ(Const(42).eval(), 42);

    Const one(1);
    EXPECT_EQ(one.eval(), 1);
    EXPECT_EQ(one.eval({{"x", 10}}), 1);
}

TEST(ExpressionTest, variable)
{
    EXPECT_EQ(Variable("x").eval({{"x", 5}}), 5);

    std::map<std::string, Number> values{{"лол", 42},
                                         {"кек", 52},
                                         {"abobus", 62}};
    EXPECT_EQ(Variable("кек").eval(values), 52);

    EXPECT_ANY_THROW(Variable("x").eval()) << "Hint: use map::at()";
}

namespace {
struct UnaryOperationTest : ::testing::TestWithParam<Const>
{
};

struct BinaryOperationTest : ::testing::TestWithParam<std::tuple<Const, Const>>
{
};

const auto CONSTS = ([](auto... values) {
    return ::testing::Values(Const(values)...);
})(-5, -2, 2, 7, 42);
} // namespace

INSTANTIATE_TEST_SUITE_P(UnaryOperationTest, UnaryOperationTest, CONSTS);
INSTANTIATE_TEST_SUITE_P(BinaryOperationTest, BinaryOperationTest, ::testing::Combine(CONSTS, CONSTS));

TEST_P(UnaryOperationTest, negate)
{
    auto & arg = GetParam();

    EXPECT_EQ((-arg).eval(), -arg.eval());
    EXPECT_EQ((-Const(arg)).eval(), -arg.eval());

    EXPECT_EQ(-(-(-(-arg))).eval(), arg.eval());
}

TEST_P(BinaryOperationTest, add)
{
    auto & [x, y] = GetParam();
    auto [xc, yc] = GetParam();
    auto result = x.eval() + y.eval();

    EXPECT_EQ((x + y).eval(), result);
    EXPECT_EQ((xc + y).eval(), result);
    EXPECT_EQ((x + yc).eval(), result);
    EXPECT_EQ((xc + yc).eval(), result);
}

TEST_P(BinaryOperationTest, subtract)
{
    auto & [x, y] = GetParam();
    auto [xc, yc] = GetParam();
    auto result = x.eval() - y.eval();

    EXPECT_EQ((x - y).eval(), result);
    EXPECT_EQ((xc - y).eval(), result);
    EXPECT_EQ((x - yc).eval(), result);
    EXPECT_EQ((xc - yc).eval(), result);
}

TEST_P(BinaryOperationTest, multiply)
{
    auto & [x, y] = GetParam();
    auto [xc, yc] = GetParam();
    auto result = x.eval() * y.eval();

    EXPECT_EQ((x * y).eval(), result);
    EXPECT_EQ((xc * y).eval(), result);
    EXPECT_EQ((x * yc).eval(), result);
    EXPECT_EQ((xc * yc).eval(), result);
}

TEST_P(BinaryOperationTest, divide)
{
    auto & [x, y] = GetParam();
    auto [xc, yc] = GetParam();
    auto result = x.eval() / y.eval();

    EXPECT_EQ((x / y).eval(), result);
    EXPECT_EQ((xc / y).eval(), result);
    EXPECT_EQ((x / yc).eval(), result);
    EXPECT_EQ((xc / yc).eval(), result);
}

namespace {
struct CompositeExpressionTest : ::testing::Test
{
    using Traits = ExpressionTraits<Number>;

    std::mt19937 & randomGenerator()
    {
        // Default seed (5489u) so that tests are deterministic
        static std::mt19937 random; // NOLINT
        return random;
    }

    Number randomNumber()
    {
        return Traits::randomNumber(randomGenerator());
    }

    bool randomBool(double likelihood)
    {
        std::bernoulli_distribution dist(likelihood);
        return dist(randomGenerator());
    }

    template <class It>
    typename std::iterator_traits<It>::reference randomElement(It begin, It end)
    {
        std::uniform_int_distribution<std::size_t> dist(0, std::distance(begin, end) - 1);
        return *std::next(begin, dist(randomGenerator()));
    }

    template <class T, std::size_t Size>
    const T & randomElement(T const (&array)[Size])
    {
        return randomElement(std::begin(array), std::end(array));
    }

    struct Options
    {
        double binaryOpLikelihood = .7;
        double constLikelihood = .5;
    };

    std::pair<Expression *, Number> randomExpressionRaw(std::size_t level, const std::map<std::string, Number> & variables, const Options & options)
    {
        if (level > 1) {
            auto [leftExpr, leftValue] = randomExpression(level - 1, variables, options);

            if (randomBool(options.binaryOpLikelihood)) {
                auto [rightExpr, rightValue] = randomExpression(level - 1, variables, options);

                std::set<const Traits::BinaryOperation *> validOperations;
                for (auto & op : Traits::BINARY_OPERATIONS) {
                    if (op.checkArgs(leftValue, rightValue)) {
                        validOperations.insert(&op);
                    }
                }

                if (!validOperations.empty()) {
                    auto & op = *randomElement(validOperations.begin(), validOperations.end());
                    return {op(*leftExpr, *rightExpr), op(leftValue, rightValue)};
                }
            }

            std::set<const Traits::UnaryOperation *> validOperations;
            for (auto & op : Traits::UNARY_OPERATIONS) {
                if (op.checkArgs(leftValue)) {
                    validOperations.insert(&op);
                }
            }

            if (!validOperations.empty()) {
                auto & op = *randomElement(validOperations.begin(), validOperations.end());
                return {op(*leftExpr), op(leftValue)};
            }
        }

        if (variables.empty() || randomBool(options.constLikelihood)) {
            Number number = randomNumber();
            return {new Const(number), number};
        }

        const std::string & var = randomElement(variables.begin(), variables.end()).first;
        return {new Variable(var), variables.at(var)};
    }

    std::pair<std::unique_ptr<Expression>, Number> randomExpression(std::size_t level, const std::map<std::string, Number> & variables, const Options & options)
    {
        auto [expr, value] = randomExpressionRaw(level, variables, options);
        return {std::unique_ptr<Expression>(expr), value};
    }
};

Number operator""_n(unsigned long long n)
{
    return n;
}
} // namespace

TEST_F(CompositeExpressionTest, simple)
{
    Variable x("x"), y("y");
    Const zero(0), one(1), two(2);
    Add sum = one + x;
    auto expr = -((x * sum) + y) / two + zero / y + sum;
    EXPECT_EQ(expr.eval({{"x", 2}, {"y", 3}}), -((2_n * (1_n + 2_n)) + 3_n) / 2_n + 0_n / 3_n + (1_n + 2_n));

    std::ostringstream ss1, ss2;
    ss1 << static_cast<const Expression &>(expr);
    ss2 << "((((-((x * (" << 1_n << " + x)) + y)) / " << 2_n << ") + (" << 0_n << " / y)) + (" << 1_n << " + x))";
    EXPECT_EQ(ss1.str(), ss2.str());
}

TEST_F(CompositeExpressionTest, no_vars)
{
    for (std::size_t i = 0; i < 10; ++i) {
        auto [expr, value] = randomExpression(5, {}, {});
        ASSERT_EQ(expr->eval(), value);
    }
}

TEST_F(CompositeExpressionTest, with_vars)
{
    for (std::size_t i = 0; i < 10; ++i) {
        std::map<std::string, Number> values;
        for (char c = 'x'; c <= 'z'; ++c) {
            values.try_emplace(std::string(1, c), randomNumber());
        }

        auto [expr, value] = randomExpression(5, values, {});

        ASSERT_EQ(expr->eval(values), value);
    }
}

TEST_F(CompositeExpressionTest, large)
{
    for (std::size_t i = 0; i < 10; ++i) {
        std::map<std::string, Number> values;
        for (char c = 'a'; c <= 'z'; ++c) {
            values.try_emplace(std::string(1, c), randomNumber());
        }

        auto [expr, value] = randomExpression(10, values, {});

        ASSERT_EQ(expr->eval(values), value);
    }
}

TEST_F(CompositeExpressionTest, chain)
{
    for (std::size_t i = 0; i < 100; ++i) {
        std::map<std::string, Number> values;
        for (char c = 'x'; c <= 'z'; ++c) {
            values.try_emplace(std::string(1, c), randomNumber());
        }

        auto [expr, value] = randomExpression(4000, values, {0, .2});

        ASSERT_EQ(expr->eval(values), value);
    }
}

TEST_F(CompositeExpressionTest, performance)
{
    std::set<std::string> variables;
    std::uniform_int_distribution<std::size_t> variableLengthDist(1, 10);
    std::uniform_int_distribution<char> charDist('a', 'z');

    while (variables.size() != 100) {
        auto length = variableLengthDist(randomGenerator());
        std::string variable;
        std::generate_n(std::back_inserter(variable), length, [&]() {
            return charDist(randomGenerator());
        });
        variables.insert(std::move(variable));
    }

    for (std::size_t i = 0; i < 100; ++i) {
        std::map<std::string, Number> values;
        for (const auto & var : variables) {
            values.try_emplace(var, randomNumber());
        }

        auto [expr, value] = randomExpression(20, values, {});

        ASSERT_EQ(expr->eval(values), value);
    }
}
