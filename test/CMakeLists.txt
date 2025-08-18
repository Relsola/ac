file(GLOB TEST_SRC "${CMAKE_CURRENT_SOURCE_DIR}/*.c")

set(ALL_TEST_TARGETS)

foreach(test_src ${TEST_SRC})
    get_filename_component(test_name ${test_src} NAME_WE)
    set(test_s "${CMAKE_SOURCE_DIR}/test/${test_name}.s")
    set(test_exe "${CMAKE_SOURCE_DIR}/test/${test_name}.exe")

    # 1. 预处理并用主编译器生成汇编
    add_custom_command(
        OUTPUT ${test_s}
        COMMAND ${CMAKE_C_COMPILER} -E -P -C ${test_src} | $<TARGET_FILE:ac> -o ${test_s} -
        DEPENDS ${test_src} $<TARGET_FILE:ac>
        COMMENT "Generating ${test_s}"
        VERBATIM
    )

    # 2. 汇编生成可执行文件
    add_custom_command(
        OUTPUT ${test_exe}
        COMMAND ${CMAKE_C_COMPILER} -o ${test_exe} ${test_s} -xc ${CMAKE_CURRENT_SOURCE_DIR}/common
				DEPENDS ${test_s} ${CMAKE_CURRENT_SOURCE_DIR}/common
        COMMENT "Linking ${test_exe}"
        VERBATIM
    )

		add_custom_target(${test_name}_run
		    ALL DEPENDS ${test_exe}
        COMMAND ${CMAKE_SOURCE_DIR}/test/${test_name}.exe
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "Running test"
		)

		list(APPEND ALL_TEST_TARGETS ${test_name}_run)
endforeach()

add_custom_target(driver
    COMMAND bash ${CMAKE_SOURCE_DIR}/test/driver.sh
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Running test-driver"
)

list(APPEND ALL_TEST_TARGETS driver)

add_custom_target(all_tests DEPENDS ${ALL_TEST_TARGETS})

add_custom_target(clear
    COMMAND rm -f ${CMAKE_SOURCE_DIR}/test/*.exe ${CMAKE_SOURCE_DIR}/test/*.s
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Remove temporary files"
)
